//! Parquet FFI for Haskell telemetry
//!
//! Provides a C-compatible interface for writing telemetry events to Parquet files.
//! Designed for high-throughput append-only workloads with fsync durability.

use arrow::array::{
    ArrayRef, Int32Builder, Int64Builder, StringBuilder, TimestampMicrosecondBuilder,
};
use arrow::datatypes::{DataType, Field, Schema, TimeUnit};
use arrow::record_batch::RecordBatch;
use libc::{c_char, c_int, c_void, size_t};
use parquet::arrow::ArrowWriter;
use parquet::basic::Compression;
use parquet::file::properties::WriterProperties;
use std::ffi::CStr;
use std::fs::File;
use std::ptr;
use std::slice;
use std::sync::Arc;

/// Opaque handle to a Parquet writer
pub struct ParquetWriter {
    writer: ArrowWriter<File>,
    schema: Arc<Schema>,
    // Builders for accumulating rows
    event_id: StringBuilder,
    seq: Int64Builder,
    timestamp_us: TimestampMicrosecondBuilder,
    monotonic_ns: Int64Builder,
    session_id: StringBuilder,
    project_id: StringBuilder,
    directory: StringBuilder,
    event_type: StringBuilder,
    payload: StringBuilder,
    meta_model: StringBuilder,
    meta_agent: StringBuilder,
    meta_tokens_in: Int32Builder,
    meta_tokens_out: Int32Builder,
    meta_tool_name: StringBuilder,
    meta_error: StringBuilder,
    row_count: usize,
}

/// Create the telemetry schema
fn telemetry_schema() -> Arc<Schema> {
    Arc::new(Schema::new(vec![
        Field::new("event_id", DataType::Utf8, false),
        Field::new("seq", DataType::Int64, false),
        Field::new(
            "timestamp",
            DataType::Timestamp(TimeUnit::Microsecond, Some("UTC".into())),
            false,
        ),
        Field::new("monotonic_ns", DataType::Int64, false),
        Field::new("session_id", DataType::Utf8, false),
        Field::new("project_id", DataType::Utf8, false),
        Field::new("directory", DataType::Utf8, false),
        Field::new("event_type", DataType::Utf8, false),
        Field::new("payload", DataType::Utf8, false), // JSON blob
        Field::new("meta_model", DataType::Utf8, true),
        Field::new("meta_agent", DataType::Utf8, true),
        Field::new("meta_tokens_in", DataType::Int32, true),
        Field::new("meta_tokens_out", DataType::Int32, true),
        Field::new("meta_tool_name", DataType::Utf8, true),
        Field::new("meta_error", DataType::Utf8, true),
    ]))
}

impl ParquetWriter {
    fn new(path: &str, row_group_size: usize) -> Result<Self, String> {
        let file = File::create(path).map_err(|e| format!("Failed to create file: {}", e))?;
        let schema = telemetry_schema();

        let props = WriterProperties::builder()
            .set_compression(Compression::ZSTD(Default::default()))
            .set_max_row_group_size(row_group_size)
            .build();

        let writer = ArrowWriter::try_new(file, schema.clone(), Some(props))
            .map_err(|e| format!("Failed to create writer: {}", e))?;

        Ok(Self {
            writer,
            schema,
            event_id: StringBuilder::new(),
            seq: Int64Builder::new(),
            timestamp_us: TimestampMicrosecondBuilder::new().with_timezone("UTC"),
            monotonic_ns: Int64Builder::new(),
            session_id: StringBuilder::new(),
            project_id: StringBuilder::new(),
            directory: StringBuilder::new(),
            event_type: StringBuilder::new(),
            payload: StringBuilder::new(),
            meta_model: StringBuilder::new(),
            meta_agent: StringBuilder::new(),
            meta_tokens_in: Int32Builder::new(),
            meta_tokens_out: Int32Builder::new(),
            meta_tool_name: StringBuilder::new(),
            meta_error: StringBuilder::new(),
            row_count: 0,
        })
    }

    fn append_event(
        &mut self,
        event_id: &str,
        seq: i64,
        timestamp_us: i64,
        monotonic_ns: i64,
        session_id: &str,
        project_id: &str,
        directory: &str,
        event_type: &str,
        payload: &str,
        meta_model: Option<&str>,
        meta_agent: Option<&str>,
        meta_tokens_in: Option<i32>,
        meta_tokens_out: Option<i32>,
        meta_tool_name: Option<&str>,
        meta_error: Option<&str>,
    ) {
        self.event_id.append_value(event_id);
        self.seq.append_value(seq);
        self.timestamp_us.append_value(timestamp_us);
        self.monotonic_ns.append_value(monotonic_ns);
        self.session_id.append_value(session_id);
        self.project_id.append_value(project_id);
        self.directory.append_value(directory);
        self.event_type.append_value(event_type);
        self.payload.append_value(payload);

        match meta_model {
            Some(v) => self.meta_model.append_value(v),
            None => self.meta_model.append_null(),
        }
        match meta_agent {
            Some(v) => self.meta_agent.append_value(v),
            None => self.meta_agent.append_null(),
        }
        match meta_tokens_in {
            Some(v) => self.meta_tokens_in.append_value(v),
            None => self.meta_tokens_in.append_null(),
        }
        match meta_tokens_out {
            Some(v) => self.meta_tokens_out.append_value(v),
            None => self.meta_tokens_out.append_null(),
        }
        match meta_tool_name {
            Some(v) => self.meta_tool_name.append_value(v),
            None => self.meta_tool_name.append_null(),
        }
        match meta_error {
            Some(v) => self.meta_error.append_value(v),
            None => self.meta_error.append_null(),
        }

        self.row_count += 1;
    }

    fn flush(&mut self) -> Result<(), String> {
        if self.row_count == 0 {
            return Ok(());
        }

        let columns: Vec<ArrayRef> = vec![
            Arc::new(self.event_id.finish()),
            Arc::new(self.seq.finish()),
            Arc::new(self.timestamp_us.finish()),
            Arc::new(self.monotonic_ns.finish()),
            Arc::new(self.session_id.finish()),
            Arc::new(self.project_id.finish()),
            Arc::new(self.directory.finish()),
            Arc::new(self.event_type.finish()),
            Arc::new(self.payload.finish()),
            Arc::new(self.meta_model.finish()),
            Arc::new(self.meta_agent.finish()),
            Arc::new(self.meta_tokens_in.finish()),
            Arc::new(self.meta_tokens_out.finish()),
            Arc::new(self.meta_tool_name.finish()),
            Arc::new(self.meta_error.finish()),
        ];

        let batch = RecordBatch::try_new(self.schema.clone(), columns)
            .map_err(|e| format!("Failed to create batch: {}", e))?;

        self.writer
            .write(&batch)
            .map_err(|e| format!("Failed to write batch: {}", e))?;

        self.row_count = 0;
        Ok(())
    }

    fn close(mut self) -> Result<(), String> {
        self.flush()?;
        self.writer
            .close()
            .map_err(|e| format!("Failed to close writer: {}", e))?;
        Ok(())
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// C FFI
// ═══════════════════════════════════════════════════════════════════════════

/// Create a new Parquet writer
/// Returns null on error, sets error_out if provided
#[no_mangle]
pub extern "C" fn parquet_writer_new(
    path: *const c_char,
    row_group_size: size_t,
    error_out: *mut *mut c_char,
) -> *mut c_void {
    let path = match unsafe { CStr::from_ptr(path) }.to_str() {
        Ok(s) => s,
        Err(_) => {
            set_error(error_out, "Invalid UTF-8 in path");
            return ptr::null_mut();
        }
    };

    match ParquetWriter::new(path, row_group_size) {
        Ok(writer) => Box::into_raw(Box::new(writer)) as *mut c_void,
        Err(e) => {
            set_error(error_out, &e);
            ptr::null_mut()
        }
    }
}

/// Append an event to the writer
/// Returns 0 on success, -1 on error
#[no_mangle]
pub extern "C" fn parquet_writer_append(
    writer: *mut c_void,
    event_id: *const c_char,
    seq: i64,
    timestamp_us: i64,
    monotonic_ns: i64,
    session_id: *const c_char,
    project_id: *const c_char,
    directory: *const c_char,
    event_type: *const c_char,
    payload: *const c_char,
    payload_len: size_t,
    meta_model: *const c_char,
    meta_agent: *const c_char,
    meta_tokens_in: c_int,
    meta_tokens_in_null: c_int,
    meta_tokens_out: c_int,
    meta_tokens_out_null: c_int,
    meta_tool_name: *const c_char,
    meta_error: *const c_char,
) -> c_int {
    let writer = unsafe { &mut *(writer as *mut ParquetWriter) };

    let event_id = cstr_to_str(event_id);
    let session_id = cstr_to_str(session_id);
    let project_id = cstr_to_str(project_id);
    let directory = cstr_to_str(directory);
    let event_type = cstr_to_str(event_type);

    // Payload might contain nulls, use length
    let payload = if payload.is_null() {
        ""
    } else {
        unsafe {
            let bytes = slice::from_raw_parts(payload as *const u8, payload_len);
            std::str::from_utf8_unchecked(bytes)
        }
    };

    let meta_model = cstr_to_option(meta_model);
    let meta_agent = cstr_to_option(meta_agent);
    let meta_tokens_in = if meta_tokens_in_null != 0 {
        None
    } else {
        Some(meta_tokens_in)
    };
    let meta_tokens_out = if meta_tokens_out_null != 0 {
        None
    } else {
        Some(meta_tokens_out)
    };
    let meta_tool_name = cstr_to_option(meta_tool_name);
    let meta_error = cstr_to_option(meta_error);

    writer.append_event(
        event_id,
        seq,
        timestamp_us,
        monotonic_ns,
        session_id,
        project_id,
        directory,
        event_type,
        payload,
        meta_model,
        meta_agent,
        meta_tokens_in,
        meta_tokens_out,
        meta_tool_name,
        meta_error,
    );

    0
}

/// Flush buffered rows to disk
/// Returns 0 on success, -1 on error
#[no_mangle]
pub extern "C" fn parquet_writer_flush(writer: *mut c_void, error_out: *mut *mut c_char) -> c_int {
    let writer = unsafe { &mut *(writer as *mut ParquetWriter) };
    match writer.flush() {
        Ok(()) => 0,
        Err(e) => {
            set_error(error_out, &e);
            -1
        }
    }
}

/// Close the writer and finalize the file
/// The writer pointer is invalid after this call
/// Returns 0 on success, -1 on error
#[no_mangle]
pub extern "C" fn parquet_writer_close(writer: *mut c_void, error_out: *mut *mut c_char) -> c_int {
    let writer = unsafe { Box::from_raw(writer as *mut ParquetWriter) };
    match writer.close() {
        Ok(()) => 0,
        Err(e) => {
            set_error(error_out, &e);
            -1
        }
    }
}

/// Free an error string returned by other functions
#[no_mangle]
pub extern "C" fn parquet_free_error(error: *mut c_char) {
    if !error.is_null() {
        unsafe {
            drop(std::ffi::CString::from_raw(error));
        }
    }
}

// Helper functions
fn cstr_to_str<'a>(s: *const c_char) -> &'a str {
    if s.is_null() {
        ""
    } else {
        unsafe { CStr::from_ptr(s) }.to_str().unwrap_or("")
    }
}

fn cstr_to_option<'a>(s: *const c_char) -> Option<&'a str> {
    if s.is_null() {
        None
    } else {
        Some(unsafe { CStr::from_ptr(s) }.to_str().unwrap_or(""))
    }
}

fn set_error(error_out: *mut *mut c_char, msg: &str) {
    if !error_out.is_null() {
        if let Ok(cstr) = std::ffi::CString::new(msg) {
            unsafe {
                *error_out = cstr.into_raw();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_write_parquet() {
        let path = CString::new("/tmp/test_telemetry.parquet").unwrap();
        let mut error: *mut c_char = ptr::null_mut();

        let writer = parquet_writer_new(path.as_ptr(), 1000, &mut error);
        assert!(!writer.is_null());

        let event_id = CString::new("evt_123").unwrap();
        let session_id = CString::new("ses_456").unwrap();
        let project_id = CString::new("proj_789").unwrap();
        let directory = CString::new("/home/user/project").unwrap();
        let event_type = CString::new("message.created").unwrap();
        let payload = CString::new(r#"{"foo": "bar"}"#).unwrap();

        let result = parquet_writer_append(
            writer,
            event_id.as_ptr(),
            0,
            1234567890,
            9876543210,
            session_id.as_ptr(),
            project_id.as_ptr(),
            directory.as_ptr(),
            event_type.as_ptr(),
            payload.as_ptr(),
            payload.as_bytes().len(),
            ptr::null(), // meta_model
            ptr::null(), // meta_agent
            0,
            1, // meta_tokens_in (null)
            0,
            1,           // meta_tokens_out (null)
            ptr::null(), // meta_tool_name
            ptr::null(), // meta_error
        );
        assert_eq!(result, 0);

        let result = parquet_writer_close(writer, &mut error);
        assert_eq!(result, 0);
    }
}

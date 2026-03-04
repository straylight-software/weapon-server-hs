#ifndef PARQUET_FFI_H
#define PARQUET_FFI_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Opaque handle to a Parquet writer
 */
typedef void* ParquetWriterHandle;

/**
 * Create a new Parquet writer
 * 
 * @param path Path to the output file
 * @param row_group_size Number of rows per row group
 * @param error_out If non-NULL, receives error message on failure (must be freed with parquet_free_error)
 * @return Writer handle, or NULL on error
 */
ParquetWriterHandle parquet_writer_new(
    const char* path,
    size_t row_group_size,
    char** error_out
);

/**
 * Append a telemetry event to the writer
 * 
 * @param writer Writer handle
 * @param event_id Unique event ID (ULID)
 * @param seq Session-local sequence number
 * @param timestamp_us UTC timestamp in microseconds since epoch
 * @param monotonic_ns Monotonic nanoseconds
 * @param session_id Session ID
 * @param project_id Project ID
 * @param directory Working directory
 * @param event_type Event type string
 * @param payload JSON payload
 * @param payload_len Length of payload in bytes
 * @param meta_model Model ID (NULL if not set)
 * @param meta_agent Agent name (NULL if not set)
 * @param meta_tokens_in Input token count
 * @param meta_tokens_in_null 1 if meta_tokens_in is NULL
 * @param meta_tokens_out Output token count
 * @param meta_tokens_out_null 1 if meta_tokens_out is NULL
 * @param meta_tool_name Tool name (NULL if not set)
 * @param meta_error Error message (NULL if not set)
 * @return 0 on success, -1 on error
 */
int parquet_writer_append(
    ParquetWriterHandle writer,
    const char* event_id,
    int64_t seq,
    int64_t timestamp_us,
    int64_t monotonic_ns,
    const char* session_id,
    const char* project_id,
    const char* directory,
    const char* event_type,
    const char* payload,
    size_t payload_len,
    const char* meta_model,
    const char* meta_agent,
    int meta_tokens_in,
    int meta_tokens_in_null,
    int meta_tokens_out,
    int meta_tokens_out_null,
    const char* meta_tool_name,
    const char* meta_error
);

/**
 * Flush buffered rows to disk
 * 
 * @param writer Writer handle
 * @param error_out If non-NULL, receives error message on failure
 * @return 0 on success, -1 on error
 */
int parquet_writer_flush(
    ParquetWriterHandle writer,
    char** error_out
);

/**
 * Close the writer and finalize the file
 * The writer handle is invalid after this call
 * 
 * @param writer Writer handle
 * @param error_out If non-NULL, receives error message on failure
 * @return 0 on success, -1 on error
 */
int parquet_writer_close(
    ParquetWriterHandle writer,
    char** error_out
);

/**
 * Free an error string returned by other functions
 * 
 * @param error Error string to free (may be NULL)
 */
void parquet_free_error(char* error);

#ifdef __cplusplus
}
#endif

#endif /* PARQUET_FFI_H */

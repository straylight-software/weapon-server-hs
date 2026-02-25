# API Cookbook

Practical examples for common tasks using the Weapon server HTTP API. All examples
use `curl` and assume the server is running on `localhost:4096`.

## Setup

Start the server:

```bash
cabal run exe:weapon-server
```

For examples that need a session, create one first and export the ID:

```bash
export SESSION=$(curl -s -X POST http://localhost:4096/session \
  -H "Content-Type: application/json" \
  -d '{"title": "My Session"}' | jq -r '.id')
```

______________________________________________________________________

## Sessions

### Create a New Session

```bash
curl -X POST http://localhost:4096/session \
  -H "Content-Type: application/json" \
  -d '{"title": "Debug Authentication"}'
```

### List All Sessions

```bash
curl http://localhost:4096/session
```

### Filter Sessions by Search Term

```bash
curl "http://localhost:4096/session?search=authentication"
```

### Filter Sessions Updated After Timestamp

```bash
# Sessions updated in the last hour
curl "http://localhost:4096/session?start=$(($(date +%s) - 3600))"
```

### Fork a Session from a Specific Message

```bash
curl -X POST "http://localhost:4096/session/$SESSION/fork" \
  -H "Content-Type: application/json" \
  -d '{"messageID": "msg_abc123"}'
```

### Delete a Session

```bash
curl -X DELETE "http://localhost:4096/session/$SESSION"
```

______________________________________________________________________

## Messaging

### Send a Prompt

```bash
curl -X POST "http://localhost:4096/session/$SESSION/message" \
  -H "Content-Type: application/json" \
  -d '{
    "parts": [{
      "type": "text",
      "text": "Explain how the authentication system works"
    }]
  }'
```

### Send a Prompt with Multiple Parts

```bash
curl -X POST "http://localhost:4096/session/$SESSION/message" \
  -H "Content-Type: application/json" \
  -d '{
    "parts": [
      {"type": "text", "text": "Review this code:"},
      {"type": "text", "text": "function add(a, b) { return a + b; }"}
    ]
  }'
```

### Send an Async Prompt (Returns Immediately)

Useful for fire-and-forget scenarios where you'll monitor via SSE:

```bash
curl -X POST "http://localhost:4096/session/$SESSION/prompt_async" \
  -H "Content-Type: application/json" \
  -d '{
    "parts": [{"type": "text", "text": "Run the test suite"}]
  }'
```

### Execute a Slash Command

```bash
curl -X POST "http://localhost:4096/session/$SESSION/command" \
  -H "Content-Type: application/json" \
  -d '{"command": "/help"}'
```

### List Messages in a Session

```bash
curl "http://localhost:4096/session/$SESSION/message"
```

### Get a Specific Message

```bash
curl "http://localhost:4096/session/$SESSION/message/msg_abc123"
```

### Abort Current Operation

```bash
curl -X POST "http://localhost:4096/session/$SESSION/abort"
```

______________________________________________________________________

## Server-Sent Events (SSE)

### Subscribe to All Events

```bash
curl -N -H "Accept: text/event-stream" http://localhost:4096/global/event
```

### Subscribe to Events for a Specific Directory

```bash
curl -N -H "Accept: text/event-stream" \
  "http://localhost:4096/global/event?directory=/home/user/my-project"
```

### Parse SSE Events with jq

```bash
curl -N -H "Accept: text/event-stream" http://localhost:4096/global/event | \
  while read -r line; do
    if [[ $line == data:* ]]; then
      echo "${line#data: }" | jq .
    fi
  done
```

### Watch for Session Status Changes

```bash
curl -N -H "Accept: text/event-stream" http://localhost:4096/global/event | \
  grep --line-buffered "session.status" | \
  while read -r line; do
    echo "${line#data: }" | jq -r '"\(.properties.sessionID): \(.properties.status)"'
  done
```

______________________________________________________________________

## Permissions

### List Pending Permission Requests

```bash
curl http://localhost:4096/permission
```

### Approve a Permission Request

```bash
curl -X POST http://localhost:4096/permission/perm_abc123/reply \
  -H "Content-Type: application/json" \
  -d '{"allowed": true}'
```

### Deny a Permission Request

```bash
curl -X POST http://localhost:4096/permission/perm_abc123/reply \
  -H "Content-Type: application/json" \
  -d '{"allowed": false}'
```

### Auto-Approve Permissions (Scripted)

```bash
# Watch for permission requests and auto-approve read operations
curl -N -H "Accept: text/event-stream" http://localhost:4096/global/event | \
  grep --line-buffered "permission.asked" | \
  while read -r line; do
    data="${line#data: }"
    tool=$(echo "$data" | jq -r '.properties.tool')
    id=$(echo "$data" | jq -r '.properties.id')
    
    if [[ "$tool" == "read" ]]; then
      curl -X POST "http://localhost:4096/permission/$id/reply" \
        -H "Content-Type: application/json" \
        -d '{"allowed": true}'
    fi
  done
```

______________________________________________________________________

## Questions

### List Pending Questions

```bash
curl http://localhost:4096/question
```

### Answer a Question

```bash
curl -X POST http://localhost:4096/question/q_abc123/reply \
  -H "Content-Type: application/json" \
  -d '{"answer": ["React"]}'
```

### Reject a Question

```bash
curl -X POST http://localhost:4096/question/q_abc123/reject
```

______________________________________________________________________

## Files and Search

### List Files in a Directory

```bash
curl "http://localhost:4096/file?path=/home/user/project/src"
```

### Read File Contents

```bash
curl "http://localhost:4096/file/content?path=/home/user/project/src/main.hs"
```

### Search File Contents

```bash
curl "http://localhost:4096/find?query=TODO&include=*.hs&limit=20"
```

### Search for Files by Name

```bash
curl "http://localhost:4096/find/file?query=Main&type=file&limit=10"
```

### Search for Symbols

```bash
curl "http://localhost:4096/find/symbol?query=handleRequest"
```

______________________________________________________________________

## Configuration

### Get Current Configuration

```bash
curl http://localhost:4096/config
```

### Get Global Configuration

```bash
curl http://localhost:4096/global/config
```

### Update Configuration

```bash
curl -X PATCH http://localhost:4096/config \
  -H "Content-Type: application/json" \
  -d '{
    "defaults": {
      "provider": "anthropic",
      "model": "claude-sonnet-4-20250514"
    }
  }'
```

______________________________________________________________________

## PTY (Terminal Sessions)

### Create a PTY Session

```bash
curl -X POST http://localhost:4096/pty \
  -H "Content-Type: application/json" \
  -d '{
    "command": "/bin/bash",
    "rows": 24,
    "cols": 80
  }'
```

### List PTY Sessions

```bash
curl http://localhost:4096/pty
```

### Resize a PTY

```bash
curl -X PUT http://localhost:4096/pty/pty_abc123 \
  -H "Content-Type: application/json" \
  -d '{"rows": 40, "cols": 120}'
```

### Get PTY Changes (Sandbox Mode)

```bash
curl http://localhost:4096/pty/pty_abc123/changes
```

### Commit PTY Changes to Filesystem

```bash
curl -X POST http://localhost:4096/pty/pty_abc123/commit
```

### Delete a PTY Session

```bash
curl -X DELETE http://localhost:4096/pty/pty_abc123
```

______________________________________________________________________

## Projects

### List All Projects

```bash
curl http://localhost:4096/project
```

### Get Current Project

```bash
curl http://localhost:4096/project/current
```

### Update Project Properties

```bash
curl -X PATCH http://localhost:4096/project/proj_abc123 \
  -H "Content-Type: application/json" \
  -d '{"name": "My Renamed Project"}'
```

______________________________________________________________________

## Providers

### List Configured Providers

```bash
curl http://localhost:4096/provider
```

### Check Provider Authentication Status

```bash
curl http://localhost:4096/provider/auth
```

______________________________________________________________________

## Tools

### List Available Tools with Schemas

```bash
curl http://localhost:4096/experimental/tool
```

### Get Tool IDs Only

```bash
curl http://localhost:4096/experimental/tool/ids
```

______________________________________________________________________

## Todos

### Get Session Todos

```bash
curl "http://localhost:4096/session/$SESSION/todo"
```

______________________________________________________________________

## Session Management

### Get Session Diff

```bash
curl "http://localhost:4096/session/$SESSION/diff"
```

### Get Diff for a Specific Message

```bash
curl "http://localhost:4096/session/$SESSION/diff?messageID=msg_abc123"
```

### Trigger Session Summarization

```bash
curl -X POST "http://localhost:4096/session/$SESSION/summarize"
```

### Revert Session

```bash
curl -X POST "http://localhost:4096/session/$SESSION/revert"
```

### Undo a Revert

```bash
curl -X POST "http://localhost:4096/session/$SESSION/unrevert"
```

______________________________________________________________________

## Health and Lifecycle

### Health Check

```bash
curl http://localhost:4096/global/health
```

Expected response:

```json
{"healthy": true, "version": "0.1.0"}
```

### Graceful Shutdown

```bash
curl -X POST http://localhost:4096/global/dispose
```

______________________________________________________________________

## Scripting Recipes

### Create Session and Send Prompt

```bash
#!/bin/bash
SESSION=$(curl -s -X POST http://localhost:4096/session \
  -H "Content-Type: application/json" \
  -d '{"title": "Automated Task"}' | jq -r '.id')

curl -X POST "http://localhost:4096/session/$SESSION/message" \
  -H "Content-Type: application/json" \
  -d '{
    "parts": [{"type": "text", "text": "'"$1"'"}]
  }'
```

### Wait for Session to Become Idle

```bash
wait_for_idle() {
  local session_id=$1
  while true; do
    status=$(curl -s http://localhost:4096/session/status | jq -r ".[\"$session_id\"]")
    if [[ "$status" == "idle" ]]; then
      break
    fi
    sleep 1
  done
}

# Usage
wait_for_idle "$SESSION"
echo "Session is now idle"
```

### Run a Command and Wait for Completion

```bash
run_and_wait() {
  local session_id=$1
  local prompt=$2
  
  # Send async prompt
  curl -s -X POST "http://localhost:4096/session/$session_id/prompt_async" \
    -H "Content-Type: application/json" \
    -d "{\"parts\": [{\"type\": \"text\", \"text\": \"$prompt\"}]}"
  
  # Wait for idle
  while true; do
    status=$(curl -s http://localhost:4096/session/status | jq -r ".[\"$session_id\"]")
    if [[ "$status" == "idle" ]]; then
      break
    fi
    sleep 1
  done
  
  # Return last message
  curl -s "http://localhost:4096/session/$session_id/message" | jq '.[-1]'
}
```

### Batch Process Multiple Prompts

```bash
#!/bin/bash
SESSION=$(curl -s -X POST http://localhost:4096/session \
  -H "Content-Type: application/json" \
  -d '{"title": "Batch Processing"}' | jq -r '.id')

prompts=(
  "List all TODO comments in the codebase"
  "Summarize the project architecture"
  "Find potential security issues"
)

for prompt in "${prompts[@]}"; do
  echo "Running: $prompt"
  curl -s -X POST "http://localhost:4096/session/$SESSION/message" \
    -H "Content-Type: application/json" \
    -d "{\"parts\": [{\"type\": \"text\", \"text\": \"$prompt\"}]}"
  
  # Wait for completion
  while [[ $(curl -s http://localhost:4096/session/status | jq -r ".[\"$SESSION\"]") != "idle" ]]; do
    sleep 1
  done
done
```

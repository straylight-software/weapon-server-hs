{
  perSystem =
    {
      inputs',
      pkgs,
      self',
      ...
    }:
    let
      withUI = pkgs.writeShellApplication {
        name = "weapon-with-server";
        runtimeInputs = [
          inputs'.weapon.packages.default
          self'.packages.default
          pkgs.curl
          pkgs.coreutils
        ];
        text = ''
          SERVER_URL="http://localhost:4096"
          LOG_DIR="''${XDG_STATE_HOME:-$HOME/.local/state}/weapon"
          LOG_FILE="$LOG_DIR/server.log"

          mkdir -p "$LOG_DIR"

          # Start server in background, log to file
          weapon-server >>"$LOG_FILE" 2>&1 &
          SERVER_PID=$!

          cleanup() {
            kill "$SERVER_PID" 2>/dev/null || true
          }
          trap cleanup EXIT

          # Wait for server to be ready (max 10 seconds)
          for _ in $(seq 1 100); do
            if curl -sf "$SERVER_URL/global/health" >/dev/null 2>&1; then
              break
            fi
            if ! kill -0 "$SERVER_PID" 2>/dev/null; then
              echo "Server failed to start. Check $LOG_FILE" >&2
              exit 1
            fi
            sleep 0.1
          done

          if ! curl -sf "$SERVER_URL/global/health" >/dev/null 2>&1; then
            echo "Server not ready after 10 seconds. Check $LOG_FILE" >&2
            exit 1
          fi

          # Run the TUI frontend - trap will clean up server on exit
          weapon attach "$SERVER_URL" "$@"
        '';
      };
    in
    {
      packages.ui = withUI;
      checks.ui = withUI;
    };
}

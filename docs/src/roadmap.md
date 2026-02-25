# Roadmap

## Current Status

The Weapon Server has achieved **full API compliance** with the OpenAPI specification:

- **95 endpoints** implemented
- **1300+ tests** passing
- **100% Stan health**

## Completed Milestones

### API Compliance

- Full endpoint parity with TypeScript server
- All CRUD operations for sessions, messages, projects
- PTY management with WebSocket support
- Server-Sent Events for real-time updates

### Infrastructure

- Custom `io_uring` HTTP server (`evring-wai`)
- Dhall-based configuration system
- Property-based test suite with Hedgehog
- NixOS module for deployment

### Agent System

- Built-in agents (armed, locked, explore, general)
- Permission-based tool access control
- Environment context injection
- Functional programming philosophy in prompts

## Upcoming Work

### Performance

- [ ] Connection pooling for LLM providers
- [ ] Response caching for repeated queries
- [ ] Profiling and optimization pass

### Features

- [ ] MCP (Model Context Protocol) server support
- [ ] Custom tool registration via config
- [ ] Session branching and forking
- [ ] Conversation compaction

### Developer Experience

- [ ] OpenAPI spec generation from Servant types
- [ ] Client SDK generation
- [ ] Improved error messages
- [ ] Request/response logging

### Documentation

- [ ] Tutorial: Building a custom agent
- [ ] Tutorial: Adding a new LLM provider
- [ ] API cookbook with examples
- [ ] Video walkthroughs

## Contributing

See [Contributing](./contributing.md) for how to get involved.

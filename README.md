# pen-it-down – Offline-First Markdown Notes (OCaml + Irmin)

Penit is an offline-first Markdown note-taking web application built entirely in **OCaml**.

![Demo](./Note_App.mp4)

It uses:

- **Irmin (Git backend)** for versioned data storage
- **WebSocket sync** for real-time store synchronization
- **Dream** as backend server
- **Brr + Js_of_ocaml** for frontend UI
- **Lwt** for asynchronous programming

---

## Features

- Create Markdown notes
- Delete notes with confirmation
- Automatic store synchronization
- Git-backed history via Irmin
- Offline-first architecture
- WebSocket-based client-server sync
- Fully written in OCaml (frontend + backend)

---

## Offline-First Design

Penit follows an offline-first model:

1. Notes are stored locally in the client Irmin store
2. Changes are pushed to the server
3. Server synchronizes via Git backend
4. Client pulls updated state
5. UI re-renders

This ensures:

- Local-first UX
- Resilience to network failures
- Git-backed version history

---

## How to setup the project

- clone the repository
- Run `make install` to install all project dependencies
- Run `make start` to build and start the application
- Open the `dist/index.html` file in the browser to view the project frontend

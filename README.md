# dmcc: AVAYA library for Haskell

[![Build & test](https://github.com/f-me/dmcc/workflows/Build%20&%20test/badge.svg)](https://github.com/f-me/dmcc/actions)
[![Hackage](https://img.shields.io/hackage/v/dmcc.svg?colorB=5e5184&style=flat)](https://hackage.haskell.org/package/dmcc)
[![Hackage deps](https://img.shields.io/hackage-deps/v/dmcc.svg)](http://packdeps.haskellers.com/feed?needle=dmcc)

This package contains a Haskell library which may be used to implement
computer telephony integration using [AVAYA DMCC XML API][dmcc-api]. A
simple server (dmcc-ws) built atop the library is also included. The
server allows clients use JSON-over-WebSockets to control AVAYA
agents, receive event notifications and agent state updates.

The package uses third-party call control functions of DMCC API.
There's no first-party call control support and no access to media
streams.

AVAYA DMCC XML API is largely based on [ECMA-354][] (CSTA Phase III)
standard, so in theory the package can be used with other compliant
telephony solutions.

## Features

- First-party interface for most of call control functions (making and
  answering calls, hold, conference call, transfer, barge in, state
  control).

- State change events (implemented in the library using polling to
  compensate for the lack of a native implementation in DMCC 6.x).

- Webhook support (the library can send HTTP requests in response to
  agent state change events).

## Site-specific notes

One basic TSAPI license is consumed for every agent controlled by
the library.

DMCC 6.x is supported. Consult your Avaya AES administration page to
check for software versions and available licenses.

## dmcc-ws server

The server exposes portions of Haskell library interface via
WebSockets using JSON messages for client-server exchange. Its purpose
is to provide a clean agent-centric interface to DMCC API suitable for
usage from client applications running in a browser.

Run the server as

```
dmcc-ws dmcc-ws.cfg
```

See example configuration file at `dmcc-ws/example.cfg`.

With `dmcc-ws` running, you may start controlling an agent with
extension `XXX` by connecting to WebSocket URL `http://host:port/XXX`.
The server accepts client commands in JSON:

```json
{"action":"MakeCall", "number":"989150603267"}
```

Consult [`DMCC.Action`][dmcc-actions] documentation in Haddock docs for DMCC library
for supported commands.

The server reports telephony events along with updated agent snapshot:

```json
{
  "newSnapshot": {
    "state": ["Busy", ""],
    "calls": {
      "179": {
        "failed": false,
        "held": false,
        "ucid": "00001001791428242051",
        "interlocutors": [
          "989150603267:ADACs8300::0"
        ],
        "start": "2015-04-05T13:52:52.803Z",
        "direction": {
          "contents": [],
          "dir": "Out"
        },
        "answered": "2015-04-05T13:53:02.686Z"
      }
    }
  },
  "dmccEvent": {
    "callId": "179",
    "event": "EstablishedEvent"
  },
  "tag": "TelephonyEvent"
}
```

Client applications may use events to update their UI incrementally or
re-process the whole state every time an event arrives.

## Build

Use [Haskell Stack](https://www.haskellstack.org/) tool to build the
library and `dmcc-ws`:

```bash
stack install
```

### Developing on macOS

On macOS with `openssl` installed via Homebrew, build with

```bash
stack build --extra-include-dirs=/usr/local/opt/openssl/include/ --extra-lib-dirs=/usr/local/opt/openssl/lib/
```

[dmcc-api]: https://www.devconnectprogram.com/site/global/products_resources/avaya_aura_application_enablement_services/interfaces/dmcc/overview/index.gsp

[ecma-354]: http://www.ecma-international.org/publications/standards/Ecma-354.htm

[dmcc-actions]: https://hackage.haskell.org/package/dmcc/docs/DMCC-Agent.html#t:Action

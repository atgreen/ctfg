# ctfg

This is a simple Capture-The-Flag game engine.

![alt text](./example.png)


## Building and Running

`ctfg` can run locally on your machine or in a container.

### On your machine

You will need to install a few dependencies first.  If you are running homebrew,
run...
```sh
brew install make sbcl ocicl gcc@11
```

Otherwise, install `sbcl` from your OS distribution, and then `ocicl` from source at https://github.com/ocicl/ocicl.

Once `ocicl` is available, run `ocicl install` to download the Common
Lisp dependencies.

And then...

* To build `ctfg`: `make`
* To test `ctfg`: `make check`
* To run `ctfg`: `ctfg --help`

```
NAME:
  ctfg - A Capture-The-Flag Game Engine

USAGE:
  ctfg

OPTIONS:
      --help                   display usage information and exit
      --version                display version and exit
  -b, --dbdir <VALUE>          database directory [default: .]
  -d, --developer-mode         enable developer mode
  -p, --port <INT>             port [default: 8080]
  -s, --slynk-port <INT>       slynk-port
  -w, --websocket-url <VALUE>  websocket-url [default: ws://localhost:12345/scorestream]

EXAMPLES:

  Run web service on port 9090:

    ctfg -p 9090

AUTHORS:
  Anthony Green

LICENSE:
  MIT
```

### In a container

First, build the image:

```sh
# docker and podman can be used interchangeably here
podman build -t ctfg .
```

Afterwards, use `docker run` or `podman run` to start a new `ctfg` game server
with the options shown above:

```sh
podman run -v $PWD/credentials.csv:/data/credentials.csv \
    -v $PWD/challenges.json:/data/challenges.json \
    -v $PWD/challenges.json:/data/game-clusters.yaml \
    ctfg --help
```

Since `ctfg` creates a database to track player and game activity, moutning a
container volume to `/data` and copying game configuration into it is
recommended:

```sh
podman volume create ctfg-data
podman run --name copier -v ctfg-data:/data bash:5 sleep infinity
for f in challenges.json credentials.csv game-clusters.yaml
do podman cp "$f" "copier:/data/$f"
done
podman rm -f copier
```

---

The `--developer-mode` option disables caching of static content, and
reloads the challenges.json every time the Challenge page is
rendered.  This allows you view your changes in real time as you are
developing content.

Client browsers must establish websocket connections back to the game
engine on the `/scorestream` endpoint.  Use the `--websocket-url`
option to tell those clients what the URL is.  For instance, if you
are hosting ctfg on an OpenShift kubernetes cluster, you might create
a TLS terminated route for your ctfg service and connect to it thusly:
`-w wss://scorestream-ctfg.apps.ocp.example.com:443/scorestream`


## Configuring your Game

1. Player credentials should live in a file called `credentials.csv`.  It's a simple `username,password` csv file.

2. Challenges are defined in `challenges.json`. This should be a JSON array containing challenge objects with the following structure:

```json
{
    "id": 5,
    "title": "SQL Injection Login",
    "category": "Web",
    "difficulty": "Easy",
    "points": 150,
    "description": "Challenge description supporting both Markdown and HTML",
    "flag": "^regexp flag goes here$",
    "testflag": "exact_flag_for_testing",
    "hints": [
        {
            "id": 1,
            "text": "First hint text (supports Markdown/HTML)",
            "cost": 10
        },
        {
            "id": 2,
            "text": "Second hint reveals after first is purchased",
            "cost": 20
        }
    ],
    "requirements": [2, 3],
    "content": "Optional additional content field"
}
```

### Challenge Fields

- **id** (required): Unique integer identifier for the challenge
- **title** (required): Challenge name displayed in the UI
- **category** (required): Category for grouping challenges (e.g., "Web", "Crypto", "Forensics")
- **difficulty** (required): Difficulty level (e.g., "Easy", "Medium", "Hard")
- **points** (required): Point value awarded for solving
- **description** (required): Challenge description that supports both Markdown syntax and HTML. The marked library renders Markdown while preserving HTML tags
- **flag** (required): Regular expression pattern for validating flag submissions
- **testflag** (optional): Exact flag value used for automated testing
- **hints** (optional): Array of hint objects with:
  - **id**: Unique identifier within the challenge
  - **text**: Hint content (supports Markdown and HTML)
  - **cost**: Points deducted when hint is revealed
  - Hints are revealed sequentially - players must purchase earlier hints first
- **requirements** (optional): Array of challenge IDs that must be solved before this challenge becomes available
- **content** (optional): Additional content field for extended challenge information

### Text Formatting

Both challenge descriptions and hint texts support:
- **Markdown**: Headers, bold/italic, code blocks, tables, lists, blockquotes
- **HTML**: Direct HTML tags like `<br>`, `<strong>`, `<em>`, `<mark>`, `<code>`
- **Mixed content**: Markdown and HTML can be used together

### Dynamic Placeholders

The following placeholders in challenge descriptions are automatically replaced at runtime:

- **@USERNAME@**: The player's login username
- **@USERID@**: The player's numeric user ID
- **@DISPLAYNAME@**: The player's chosen display name (or "[unset]" if not configured)
- **@OBFUSCATED_DISPLAYNAME@**: An XOR-masked and checksummed version of the display name (for anti-cheating purposes)
- **@CONTROL_CLUSTER@**: The control Kubernetes cluster from game-clusters.yaml
- **@PLAYER_CLUSTER@**: The player's assigned Kubernetes cluster (assigned round-robin from the player clusters list)

3. Replace `images/banner.png` with your own content.

4. Edit `game-clusters.yaml` to point at the Kubernetes cluster hosting this app,
   as well as the list of player clusters (all possibly the same).
   Users are assigned to the different player clusters in a
   round-robin format as they join.

## Load Testing

For testing server performance under high concurrent load, use the included `player-emulator.js`:

```bash
# Test with N concurrent players (requires Node.js and npm install)
./player-emulator.js <server_url> challenges.json credentials.csv <N>

# Examples:
./player-emulator.js http://localhost:8080 challenges.json credentials.csv 70
./player-emulator.js https://ctfg.example.com challenges.json credentials.csv 390
```

The emulator simulates realistic browser behavior:
- Fetches static files (HTML, CSS, JS, images)
- Logs in with credentials from CSV file
- Sets unique display names
- Establishes WebSocket connections
- Solves all available challenges using testflags
- Validates WebSocket messages and logs detailed progress

Perfect for stress testing before game day!

## API

Most REST endpoints in ctfg are intended for use by the browser
client.  However, ctfg does provide one endpoint intended for use
by an external non-browser client.

Posting to the `/api/award` endpoint emulates a successful flag
submission for a specific `username` and challenge `id`.  Use this API
for any automated flag submission by an external judge process.  For
example, posting the following json will tell ctfg to behave as
through player `player1` had submitted the correct flag for challenge
number `5`.

```
{
  "username": "player1",
  "id": "5"
}
```

An `AUTHORIZATION` token must be provided in the http header for this
API.  Specify this token when you launch ctfg by setting the
`CTFG_API_TOKEN` environment variable.

## Author and License

`ctfg` was written by Anthony Green and is distributed
under the terms of the MIT license.

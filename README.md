# ctfg

This is a simple Capture-The-Flag game engine.

## Building and Running

```sh
make
ctfg --help
```

## Configuring your Game

1. Player credentials should live in a file called `credentials.csv`.  It's a simple `username,password` csv file.

2. Challenges are in `src/challenges.json`.  This should be a json array containing objects like this:
```
        {
            "id": 5,
            "title": "SQL Injection Login",
            "category": "Web",
            "difficulty": "Easy",
            "points": 150,
            "description": "Bypass the login mechanism using SQL injection techniques.",
            "content": "Login page source code: \n    echo \"Login successful! Flag: \" . $flag; \n} else { \n    echo \"Invalid credentials\"; \n}" ,
            "flag": "foo",
            "requirements": [2, 3]
	      },
```

  Each challenge needs a unique `id`.  All of the other fields are self-explanatory.  The `requirements` field is optional. It should be a list of challenges that must be solved before this challenge appears on the board.

3. Replace `static/images/banner.png` with your own content.


## Author and License

`ctfg` was written by Anthony Green and is distributed
under the terms of the MIT license.

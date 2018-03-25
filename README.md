# talklicker

A small Haskell+Elm event scheduling app.

A SPA Made to organise events and days at work - one can login, add events to a list that they would like to
(mainly talks that they would like to give), and then an admin can create a Day, describe it in Markdown
and drag events into slots throughout the description.

# Installation

Client:

1. Install [Elm](http://elm-lang.org) 0.18 - There are likely to be a few breakages with newer/older versions. You can probably install this from npm if it is hard to get hold of the correct version from the Elm homepage.
2. install [NPM](https://www.npmjs.com) - NPM 3.10.10 is known to work, but we don't do much with it, so most versions should work.
3. In the `client` directory, run `npm install` to install the required packages, and then `npm run build` to build everything into the `client/build` directory.

Server:

1. Install [Stack](https://haskellstack.org)
2. In the `server` directory, run `stack install`
3. To begin the server, run something like `talklicker --static client/build --db server/talklicker.json --port 9090` in the root folder. Alternately, run `scripts/run-server.sh` to install and run using the above defaults.



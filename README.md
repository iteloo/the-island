# the-island
A group survival game that combines mobile technology with tabletop gaming, implemented using WebSockets. Currently only the client-side code exists. It's written in Elm. 

## Development

To install Elm 0.18 globally, run
```
npm install -g elm@0.18
```

### Option 1: Build Elm code
```
bash build.sh
```
This generates the javascript in `web/elm.js`. You can then view the webpage by opening `web/index.html` in your browser


### Option 2: Elm Reactor
Alternative, you can view the webpage without explicit compilation, using the Elm reactor devserver. Note shaking is not supported in this mode. 
```
cd elm
elm reactor
```
and going to `http://localhost:8000/Reactor.elm`

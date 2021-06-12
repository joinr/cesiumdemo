# cesiumdemo

An exploration of the cesiumjs api from clojurescript.  Working prototype visualizing
entities moving around, doing some realtime plotting with vega as well.

## Development Mode

### Run application:

```
lein clean
lein fig -- --build dev --repl
```

Figwheel will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:9500](http://localhost:9500).

## Production Build

```
lein clean
lein cljsbuild once min
```

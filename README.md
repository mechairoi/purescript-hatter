purescript-hatter [![Build Status](https://travis-ci.org/mechairoi/purescript-hatter.svg?branch=master)](https://travis-ci.org/mechairoi/purescript-hatter)
===
A template engine for PureScript and virtual-dom [wip]

Hatter templates are text files containing a mix of HTML markup and PureScript code.
Hatter compiler translate them into PuresSript function which returns `VirtualDOM.VTree.VTree`.

## Try
```bash
$ npm install
$ bower install
$ $(npm bin)/gulp bin
$ ./bin/hatter.js < test/integration/fixture/Test1.hat
```

## Usage

### With gulp

```bash
$ npm install --save-dev gulp mechairoi/gulp-purescript-hatter
$ bower install --save-dev mechairoi/purescript-hatter-runtime
```

and in `gulpfile.js`

```javascript
var hatter = require("gulp-purescript-hatter")
  , gulp   = require('gulp');

gulp.task('template', function() {
  return gulp.src("templates/**/*.hat")
    .pipe(hatter({imports: ["Text.Hatter.Runtime.Instances"]}))
    .pipe(gulp.dest("templates"));
});
```

See also https://github.com/mechairoi/purescript-hatter-todomvc

### With Grunt

XXX WIP

## Syntax

Top-level (not indented) codes are just PureScript.
Indented codes are templates like HTML. We can embed PureScript code into templates by `<% code %>`.

For example,

```purescript
module Sample1 where
import VirtualDOM.VTree.Typed

render :: String -> VTree
render x =
  <div><% x %></div>
```

is translated into

```purescript
module Sample1 where
import VirtualDOM.VTree.Typed

render :: String -> VTree
render = vnode "div" [] [ vtext x ] Nothing Nothing
```

### Embbeding other templates

```purescript
module Sample1 where
import VirtualDOM.VTree.Typed

render :: String -> VTree
render x =
  <span><% x %></span>

render2 :: String -> VTree
render2 x =
  <div><% render x %></div>
```

### Control flow

XXX WIP

## Module Documentation

- [Module documentation](src/Text/README.md)

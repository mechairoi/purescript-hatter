'use strict';

var gulp        = require('gulp')
  , gutil       = require('gulp-util')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , path        = require("path")
  , through     = require('through2')
  ;

var paths = {
  src: 'src/**/*.purs',
  ffiSrc: 'src/**/*.js',
  bin: 'Index.purs',
  doc: 'MODULE.md',
  bowerSrc: 'bower_components/purescript-*/src/**/*.purs',
  bowerFfiSrc: 'bower_components/purescript-*/src/**/*.js',
  dest: 'output/node_modules',
  unitTest: 'test/unit/**/*.purs',
  integrationTest: 'test/integration/**/*.purs',
  fixture: 'test/integration/fixture/**/*.hat',
  fixtureDest: 'test/integration/fixture'
};

var options = {
  bin: {
    main: 'Index',
    output: 'index.js'
  }
};

(function() {
  var nodePath = process.env.NODE_PATH;
  var buildPath = path.resolve(paths.dest);
  process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
})();

function stringSrc(filename, contents) {
  var src = require('stream').Readable({ objectMode: true });
  src._read = function () {
    this.push(new gutil.File({ cwd: "", base: "", path: filename, contents: new Buffer(contents) }));
    this.push(null);
  };
  return src;
}

gulp.task('make', function() {
  return purescript.psc({
    src: [paths.src].concat(paths.bowerSrc),
    ffi: [paths.ffiSrc].concat(paths.bowerFfiSrc),
    output: paths.dest
  });
});

gulp.task('unit-test-make', function() {
  return gulp.src([paths.src, paths.unitTest].concat(paths.bowerSrc))
    .pipe(purescript.pscMake({output: paths.dest}));
});

gulp.task('unit-test', ['unit-test-make'], function() {
  return stringSrc("test/unit-test.js", "require('UnitTest').main()")
    .pipe(run('node').exec());
});

gulp.task('integration-test-precompile', ['make'], function() {
  // XXX only if modified
  return gulp.src(paths.fixture)
    .pipe(hatter({imports: ["Text.Hatter.Runtime.Instances"]}))
    .pipe(gulp.dest(paths.fixtureDest));
});

gulp.task('integration-test-make', ['integration-test-precompile'], function() {
  return gulp.src([paths.src, paths.integrationTest].concat(paths.bowerSrc))
    .pipe(purescript.pscMake({output: paths.dest}));
});

gulp.task('integration-test', ['integration-test-make'], function() {
  return stringSrc("test/integration-test.js", "require('IntegrationTest').main()")
    .pipe(run('node').exec());
});

gulp.task('test', ['unit-test', 'integration-test']);

gulp.task('bin', function() {
  return gulp.src([paths.src, paths.bin].concat(paths.bowerSrc))
    .pipe(purescript.psc(options.bin))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('docs', function() {
  return gulp.src(paths.src)
    .pipe(purescript.pscDocs())
    .pipe(gulp.dest(paths.doc));
});

gulp.task('psci', function() {
  return gulp.src([paths.src, paths.unitTest].concat(paths.bowerSrc))
    .pipe(purescript.dotPsci());
});

gulp.task('watch', function() {
  gulp.watch(paths.src, ['make', 'docs']);
});

gulp.task('default', ['make', 'docs']);

function hatter(opts) {
  module.paths.push(paths.dest);
  return through.obj(function(file, enc, cb) {
    var PluginError = gutil.PluginError;
    var PLUGIN_NAME = 'gulp-purescript-hatter';

    if (file.isNull()) {
      return cb(null, file);
    }

    if (file.isStream()) {
      return cb(new PluginError(PLUGIN_NAME, 'Streaming not supported', {
        fileName: file.path,
        showStack: false
      }));
    }

    try {
      file.path = gutil.replaceExtension(file.path, ".purs");
      file.contents = new Buffer(
        require('Text.Hatter').hatter(opts.imports || [])(String(file.contents)).value0
      );
    } catch (e) {
      return cb(new PluginError(PLUGIN_NAME, e.message || e.msg, {
        fileName: file.path,
        lineNumber: e.line,
        stack: e.stack,
        showStack: false
      }));
    }
    return cb(null, file);
  });
};

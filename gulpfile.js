'use strict';

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , source      = require('vinyl-source-stream')
  , browserify  = require('browserify')
  ;

var paths = {
  src: 'src/**/*.purs',
  binSrc: 'Index.purs',
  bowerSrc: [
    'bower_components/purescript-*/src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs.hs'
  ],
  dest: '',
  docs: {
    'Text.Hatter': {
      dest: 'src/Text/README.md',
      src: 'src/Text/Hatter.purs'
    },
    'Text.Hatter.*': {
      dest: 'src/Text/Hatter/README.md',
      src: 'src/Text/Hatter/*.purs'
    }
  },
  test: 'examples/**/*.purs'
};

var options = {
  test: {
    main: 'Test',
    output: 'test/test.js'
  },
  pack: {
    main: 'Text.Hatter.Index',
    output: 'hatter.js'
  }
};

function compile (compiler, src, opts) {
  var psc = compiler(opts);
  psc.on('error', function(e) {
    console.error(e.message);
    psc.end();
  });
  return gulp.src(src.concat(paths.bowerSrc))
    .pipe(psc)
    .pipe(gulp.dest(paths.dest));
};

function docs (target) {
  return function() {
    var docgen = purescript.pscDocs();
    docgen.on('error', function(e) {
      console.error(e.message);
      docgen.end();
    });
    return gulp.src(paths.docs[target].src)
      .pipe(docgen)
      .pipe(gulp.dest(paths.docs[target].dest));
  };
}

function sequence () {
  var args = [].slice.apply(arguments);
  return function() {
    runSequence.apply(null, args);
  };
}

gulp.task('pack', function() {
  return compile(purescript.psc, [paths.src, paths.binSrc].concat(paths.bowerSrc), options.pack);
});

gulp.task('make', function() {
  return compile(purescript.pscMake, [paths.src].concat(paths.bowerSrc), {});
});

gulp.task('test', function() {
  return compile(purescript.psc, [paths.src, paths.test].concat(paths.bowerSrc), options.test)
    .pipe(run('node').exec());
});

gulp.task('docs-Text.Hatter', docs('Text.Hatter'));
gulp.task('docs-Text.Hatter.*', docs('Text.Hatter.*'));

gulp.task('docs', ['docs-Text.Hatter', 'docs-Text.Hatter.*']);

gulp.task('watch-make', function() {
  gulp.watch(paths.src, sequence('make', 'docs'));
});

gulp.task('default', sequence('make', 'docs'));

const config = require('./config');
const gulp = require('gulp');
const elm  = require('gulp-elm');
const minify = require('gulp-minify');

const options = { debug: true, warn: true };

gulp.task('elm-init', elm.init);

gulp.task('elm-bundle', ['elm-init'], () =>
    // separate min
    gulp.src(config.source.elm)
      .pipe(elm.bundle(`${config.output.elmApplicationName}.js`, { debug: true }))
      .pipe(minify())
      .pipe(gulp.dest(config.public.js))
);

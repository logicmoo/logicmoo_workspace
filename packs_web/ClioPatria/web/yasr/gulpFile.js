require('require-dir')('./gulp');

var gulp = require('gulp');	
gulp.task('default', ['modifyYasqeInclude', 'browserify', 'browserifyWithDeps', 'makeCss', 'makeMainPage']);
gulp.task('serve', ['modifyYasqeIncludeDev', 'makeCss', 'makeMainPage', 'browserifyForDebug', 'watch', 'connect']);


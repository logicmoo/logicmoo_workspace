var gulp = require('gulp'),
	htmlreplace = require('gulp-html-replace');


gulp.task('modifyYasqeIncludeDev', function() {
  gulp.src('index.html')
    .pipe(htmlreplace({
        'css': '//localhost/yasgui/yasqe/dist/yasqe.min.css',
        'js': '//localhost/yasgui/yasqe/dist/yasqe.bundled.js'
    },
    {keepBlockTags: true}))
    .pipe(gulp.dest('./'));
});
gulp.task('modifyYasqeInclude', function() {
	var version = "2";
	try {
		version = require('../../yasqe/package.json').version;
	} catch (e) {
		console.log("could not detect yasqe version to reference. Using YASQE version " + version + " now");
	}
	gulp.src('index.html')
	.pipe(htmlreplace({
		'js': '//cdn.jsdelivr.net/yasqe/' + version + '/yasqe.bundled.min.js',
		'css': '//cdn.jsdelivr.net/yasqe/' + version + '/yasqe.min.css'
	},
	{keepBlockTags: true}))
	.pipe(gulp.dest('./'));
});
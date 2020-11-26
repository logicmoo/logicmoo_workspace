# Cut paste commands
````bash
find ../../.. -name "*.bak" -delete

sed -e "s| function \(.*\){| function \1\n    {|g" -e "s|class \(.*\){|class \1\n{|g"  -e "s|\! |\!|g" 

git clone https://github.com/TeamSPoon/elFinder ef

[core]
        repositoryformatversion = 0
        filemode = false
        bare = false
        logallrefupdates = true
        symlinks = false
        ignorecase = true
[remote "origin"]
        url = https://github.com/TeamSPoon/elFinder
        fetch = +refs/heads/*:refs/remotes/origin/*
[remote "upstream"]
        url = https://github.com/Studio-42/elFinder/
        fetch = +refs/heads/*:refs/remotes/origin/*


git add efCloserMine/composer.json
git add efCloserMine/js/elFinder.js
git add efCloserMine/php/autoload.php
git add efCloserMine/php/elFinder.class.php
git add efCloserMine/php/elFinderConnector.class.php
git add efCloserMine/php/elFinderFlysystemGoogleDriveNetmount.php
git add efCloserMine/php/elFinderPlugin.php
git add efCloserMine/php/elFinderSession.php
git add efCloserMine/php/elFinderVolumeBox.class.php
git add efCloserMine/php/elFinderVolumeDriver.class.php
git add efCloserMine/php/elFinderVolumeDropbox2.class.php
git add efCloserMine/php/elFinderVolumeFTP.class.php
git add efCloserMine/php/elFinderVolumeGoogleDrive.class.php
git add efCloserMine/php/elFinderVolumeGroup.class.php
git add efCloserMine/php/elFinderVolumeLocalFileSystem.class.php
git add efCloserMine/php/elFinderVolumeMySQL.class.php
git add efCloserMine/php/elFinderVolumeOneDrive.class.php

git add efMine/composer.json
git add efMine/elfinder.ckeditor5.html
git add efMine/elfinder.html.orig
git add efMine/elfinder.swish.html
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy82OGU0YTNlOGI4ZmRhNzgwNWEwYzM4Yjc5ODY0ZGMzZGQwZmM5ZDhmcjEtOTYwLTUzOXYyX2hxLmpwZw1577523823.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjcgMTYuMjEuMDkuanBn1577521268.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjcgMTYuMjUuMzcuanBn1577571914.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjggMDAuNDAuNDcuanBn1577551246.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjggMDAuNDEuMDIuanBn1577551262.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjggMDAuNTAuMjguanBn1577551828.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjggMTQuMTQuMjEuanBn1577600060.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjggMTQuMTQuMzIuanBn1577600072.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvc29mdHdhcmUvZnJkY3NhLm9yZy9Ecm9wYm94LzIwMTktMTItMjggMTQuMTUuNDkuanBn1577571907.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvcGFwZXJzL29udG9zYXVydXMtc2NyZWVuc2hvdC5naWY1520144295.png
git add efMine/files/.tmb/l1_bG9naWNtb28tc3R1ZmYvcGFwZXJzL3M1LmpwZw1520122314.png
git add efMine/files/foo.pl
git add efMine/files/NewFile.txt
git add efMine/files/NewFile.txt.tar.txt
git add efMine/files/temp/log.txt
git add efMine/img/edit_aceeditor.png
git add efMine/img/edit_ckeditor.png
git add efMine/img/edit_codemirror.png
git add efMine/img/edit_creativecloud.png
git add efMine/img/edit_pixlreditor.png
git add efMine/img/edit_pixlrexpress.png
git add efMine/img/edit_tinymce.png
git add efMine/js/elFinder.js
git add efMine/js/extras/encoding-japanese.min.js
git add efMine/js/i18n/elfinder.jp.js
git add efMine/js/ui/mkdirbutton.js
git add efMine/php/autoload.php
git add efMine/php/cm.php
git add efMine/php/composer.json
git add efMine/php/composer.lock
git add efMine/php/connector.minimal - Copy.php
git add efMine/php/connector.minimal.php
git add efMine/php/connector.php
git add efMine/php/elFinder.class.php
git add efMine/php/elFinderConnector.class.php
git add efMine/php/elFinderFlysystemGoogleDriveNetmount.php
git add efMine/php/elFinderPlugin.php
git add efMine/php/elFinderSession.php
git add efMine/php/elFinderVolumeBox.class.php
git add efMine/php/elFinderVolumeDriver.class.php
git add efMine/php/elFinderVolumeDropbox.class.php
git add efMine/php/elFinderVolumeDropbox2.class.php
git add efMine/php/elFinderVolumeFTP.class.php
git add efMine/php/elFinderVolumeGoogleDrive.class.php
git add efMine/php/elFinderVolumeGroup.class.php
git add efMine/php/elFinderVolumeLocalFileSystem.class.php
git add efMine/php/elFinderVolumeMySQL.class.php
git add efMine/php/elFinderVolumeOneDrive.class.php
git add efMine/php/libs/GdBmp.php
git add efMine/php/MySQLStorage.sql
git add efMine/php/plugins/AutoResize/plugin.php
git add efMine/php/plugins/AutoRotate/plugin.php
git add efMine/php/plugins/Normalizer/plugin.php
git add efMine/php/plugins/Sanitizer/plugin.php
git add efMine/php/plugins/Watermark/plugin.php
cp  /var/www/html/icons/ . -a
 du  /var/www/html/update-site/ -h
cp /var/www/html/index.html .
cp /var/www/html/main.html .

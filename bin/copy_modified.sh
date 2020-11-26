
export FileStem=modified-files-$(date +"%Y%m%d_%H%M%S")

zip ${FileStem}.zip $(git ls-files --modified)
unzip ${FileStem}.zip -d ../${FileStem}/
mv ${FileStem}.zip  ../${FileStem}/


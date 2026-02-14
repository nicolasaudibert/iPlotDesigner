# Usage: set_localization_symlinks_iPlotDesigner.sh targetLanguage

# Delete existing symlinks if any
rm -f iPlotDesigner.html
rm -f iPlotDesigner.labels.txt

# Create new symlinks
ln -s "localization/iPlotDesigner.${1}.html" "iPlotDesigner.html"
ln -s "localization/iPlotDesigner.labels.${1}.txt" "iPlotDesigner.labels.txt"

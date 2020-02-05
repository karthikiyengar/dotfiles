mkdir -p "~/.wallpapers"
wallpaper_location="~/.wallpapers/current-wallpaper.jpg"
urls=$(wget -O - -o /dev/null http://www.reddit.com/r/earthporn/.rss | grep -oP 'https://i.redd.it/.*?jpg')

for url in $urls; do 
    wget -o /dev/null -O $wallpaper_location $url
    is_landscape=$(identify -format '%[fx:(w>h)]' $wallpaper_location)
    resolution=$(feh -L "%p" $wallpaper_location)
    is_highres=0;
    if [[ $resolution -gt 1000000 ]]; then
        is_highres=1;
    fi;
    if [[ $is_landscape = 1 && $is_highres = 1 ]]; then
        feh --bg-scale $wallpaper_location
        break;
    fi
done;

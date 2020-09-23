#!/usr/bin/env bash
# apt install imagemagick
SUBREDDITS=$(cat <<-END
earthporn/top
cityporn/top
skyporn/top
beachporn/top
waterporn/top
auroraporn/hot
exposureporn/hot
END
)

SUBREDDIT=$(echo "$SUBREDDITS" | shuf -n 1)

echo "Choosing $SUBREDDIT"
mkdir -p ~/.wallpapers
wallpaper_location=~/.wallpapers/current-wallpaper.jpg
urls=$(wget -O - -o /dev/null http://www.reddit.com/r/$SUBREDDIT/.rss |  grep -oP 'https://i.redd.it/\w{13}\.(jpg|png)')

for url in $urls; do 
    wget -O $wallpaper_location $url
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

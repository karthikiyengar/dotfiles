#!/bin/sh
# This script needs feh and wget

subreddit=$(
  shuf -n1 <<-END
earthporn/top
cityporn/top
skyporn/top
beachporn/top
waterporn/top
auroraporn/hot
exposureporn/hot
END
)

echo "Choosing $subreddit"
mkdir -p ~/.wallpapers
wallpaper_location=~/.wallpapers/current-wallpaper.jpg
urls=$(wget -O - -o /dev/null http://www.reddit.com/r/"$subreddit"/.rss | grep -oP 'https://i.redd.it/\w{13}\.(jpg|png)')
for url in $urls; do
  wget -O $wallpaper_location "$url"
  height=$(feh -L "%h" $wallpaper_location)
  width=$(feh -L "%w" $wallpaper_location)
  resolution=$(feh -L "%p" $wallpaper_location)
  if [ "$resolution" -lt 1000000 ] || [ "$width" -lt "$height" ]; then
    continue
  fi
  feh --bg-scale $wallpaper_location
  break
done

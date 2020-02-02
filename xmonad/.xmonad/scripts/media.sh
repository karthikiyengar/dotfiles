#!/bin/bash

if [ $1 == "brightness-inc" ]; then
    light -A 5; rumno -b $(light -G) 
fi

if [ $1 == "brightness-dec" ]; then
    light -U 5; rumno -b $(light -G) 
fi
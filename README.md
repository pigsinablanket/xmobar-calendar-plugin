# xmobar-calendar-plugin

A xmobar plugin to notify you on your next calendar events.

## Installing

Installation for Xmobar plugins can be found at http://hackage.haskell.org/package/xmobar-0.8/src/README.html#installingremoving-a-plugin

## Usage

`[Run Calendar "filename.ics" 60]`
where the first argument is the location of the iCalendar event file and the seconds is the number of seconds before hand to notify with a beep.

Currently it will print in ISO format such as "2020-06-16 06:00:00 PDT"

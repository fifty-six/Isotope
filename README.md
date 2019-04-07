# Isotope

A command line utility for [TJHSST's Ion](https://ion.tjhsst.edu).

It supports listing of signups, lookups of activities, signing up for activities,
viewing the schedule, and viewing the FCPS emergency message status.
It uses concurrent REST requests to speed up API calls.
Colored output is done via [pretty-simple](https://hackage.haskell.org/package/pretty-simple)
[Asciinema demo with full colored output](https://asciinema.org/a/239358)


Help Message
```
% isotope --help
Isotope: Ion CLI

Usage: isotope COMMAND
  Use isotope to manage Ion 8th period activities.

Available options:
  -h,--help                Show this help text

Available commands:
  signups                  List signups.
  location                 Get location of activity given name
  signup                   Signup for activity given block and name
  schedule                 Display schedule for today or next available day.
  emergency                Display emergency status and message if there is any.
```

Listing of signups
```
% isotope signups
[ "Activity {aid = 3088, name = "Machine Learning Club", rooms = ["Rm 67 Lab","Commons: Galileo (lst floor, front)"]}" 
, "Activity {aid = 948, name = "Study Hall (Field)", rooms = ["Rm 254"]}" 
] 
```

Looking up the location of an activity
```
% isotope location "study hall field"
Activity 
    { aid = 948
    , name = "Study Hall (Field)" 
    , rooms = [ "Rm 254" ]
    } 
```

Signing up for an activity
```
% isotope signup A shetyck
"Signed up for Study Hall (Sheptyck)"
```

Viewing the schedule
```
% isotope schedule
Schedule 
    { schedule_name = "Anchor Day" 
    , schedule_special = False
    , schedule_blocks = 
        [ ScheduleBlock 
            { scheduleBlock_order = 1
            , scheduleBlock_name = "Period 1" 
            , scheduleBlock_start = "8:40" 
            , scheduleBlock_end = "9:30" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 2
            , scheduleBlock_name = "Period 2" 
            , scheduleBlock_start = "9:40" 
            , scheduleBlock_end = "10:25" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 3
            , scheduleBlock_name = "Period 3" 
            , scheduleBlock_start = "10:35" 
            , scheduleBlock_end = "11:20" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 4
            , scheduleBlock_name = "Period 4" 
            , scheduleBlock_start = "11:30" 
            , scheduleBlock_end = "12:15" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 5
            , scheduleBlock_name = "Lunch" 
            , scheduleBlock_start = "12:15" 
            , scheduleBlock_end = "12:45" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 6
            , scheduleBlock_name = "JLC" 
            , scheduleBlock_start = "12:45" 
            , scheduleBlock_end = "13:25" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 7
            , scheduleBlock_name = "Period 5" 
            , scheduleBlock_start = "13:25" 
            , scheduleBlock_end = "14:10" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 8
            , scheduleBlock_name = "Period 6" 
            , scheduleBlock_start = "14:20" 
            , scheduleBlock_end = "15:05" 
            } 
        , ScheduleBlock 
            { scheduleBlock_order = 9
            , scheduleBlock_name = "Period 7" 
            , scheduleBlock_start = "15:15" 
            , scheduleBlock_end = "16:00" 
            } 
        ] 
    } 
```

Viewing emergency announcements
```
% isotope emergency
EmergencyMessage 
    { emergency_status = False
    , emergency_message = Nothing
    } 
```

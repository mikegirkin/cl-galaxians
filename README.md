# cl-galaxians

## Running

```sh
make run
```

Or start `sbcl` and then

```cl
(ql:quickload :galaxians)

(galaxians:main)
```

## Running tests
```cl
make test
```

## TODOs

- [x] projectiles movement
- [x] static enemies destroying
    - [x] fix `has-common-area?` func
    - [x] WONTDO: implement collision detector based on vectors
- [x] use std decart system for geometry, invert only on drawing
- [x] killing enemies
- [x] projectile rendering
- [x] enemy movement
- [x] enemy rotation on movement
- [ ] enemies should move to the closest boundary when flying from the initial point
- [ ] enemies should move at relatively similar speeds when initiate attack and when returning from it
- [ ] enemy blowup rendering
- [ ] enemy firing
- [ ] define different trajectories for different enemy types
- [ ] lives count and display
- [ ] score count and display

## Assets

PLayer ship: https://foozlecc.itch.io/void-main-ship

Enemy ships: https://opengameart.org/content/spaceships-32x32

## Links

[Practical common lisp](https://gigamonkeys.com/book/)

[Allegro 5 library docs](https://liballeg.org/a5docs/trunk/)


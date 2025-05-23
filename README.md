# cl-galaxians

## Running

```sh
sbcl --load runner.lisp
```

Or start `sbcl` and then

```cl
(ql:quickload :galaxians)

(galaxians:main)
```

## Running tests
```cl
(ql:quickload :galaxians)
(fiveam:run! 'galaxians-spec::game-state-spec)
```

## TODOs

- [x] projectiles movement
- [x] static enemies destroying
    - [x] fix `has-common-area?` func
    - [x] WONTDO: implement collision detector based on vectors
- [x] use std decart system for geometry, invert only on drawing
- [x] killing enemies
- [x] projectile rendering
- [ ] enemy movement
- [ ] enemy firing
- [ ] generalize movement code

## Assets

PLayer ship: https://foozlecc.itch.io/void-main-ship
Enemy ships: https://opengameart.org/content/spaceships-32x32

## Links

[Practical common lisp](https://gigamonkeys.com/book/)
[Allegro 5 library docs](https://liballeg.org/a5docs/trunk/)


#ifndef _COLOR_H_
#define _COLOR_H_

/*** Color definitions ***/

typedef struct {
  int red, green, blue;
  int pixel;
} color_type;

/* "VGA" standard color palette - defined constants */
#define BLACK  { 0, 0, 0, 0 }
#define BLUE  { 0, 0, 255, 0 }
#define GREEN  { 0, 255, 0, 0 }
#define CYAN  { 0, 255, 255, 0 }
#define RED   { 255, 0, 0, 0 }
#define MAGENTA  { 255, 0, 255, 0 }
#define BROWN  { 139, 69, 19, 0 }
#define LIGHTGRAY  { 211, 211, 211, 0 }
#define DARKGRAY  { 105, 105, 105, 0 }
#define LIGHTBLUE  { 173, 216, 230, 0 }
#define LIGHTGREEN  { 124, 252, 0, 0 }
#define LIGHTCYAN  { 224, 255, 255, 0 }
#define LIGHTRED  { 255, 105, 180, 0 }
#define LIGHTMAGENTA  { 238, 130, 238, 0 }
#define YELLOW  { 255, 255, 0, 0 }
#define WHITE  { 255, 255, 255, 0 }

/* "VGA" standard color palette - variables */
static color_type Black = BLACK;
static color_type Blue = BLUE;
static color_type Green = GREEN;
static color_type Cyan = CYAN;
static color_type Red  = RED;
static color_type Magenta = MAGENTA;
static color_type Brown = BROWN;
static color_type Lightgray = LIGHTGRAY;
static color_type Darkgray = DARKGRAY;
static color_type Lightblue = LIGHTBLUE;
static color_type Lightgreen = LIGHTGREEN;
static color_type Lightcyan = LIGHTCYAN;
static color_type Lightred = LIGHTRED;
static color_type Lightmagenta = LIGHTMAGENTA;
static color_type Yellow = YELLOW;
static color_type White = WHITE;


#define X16BITCOLOR(colorvalue) ((float) colorvalue / 255.0) * 65535.0


#endif /* _COLOR_H_ */

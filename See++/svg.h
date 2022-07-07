#include<stdlib.h>
#include<stdbool.h>
#include<stdio.h>
#include<math.h>

// --------------------------------------------------------
// STRUCT definitions
// --------------------------------------------------------
typedef struct svg
{
    char* svg;
    int height;
    int width;
    bool finalized;
} svg;


struct point
{
		double x;
		double y;
};


struct pixel
{
		struct point ep1;
};

struct circle
{
		double B;
		struct point A;

		double C;
		double D;




};


struct canvas_node
{
		struct canvas_node *next;
		struct pixel *ct;
};

struct canvascircle_node
{
		struct canvascircle_node *next;
		struct circle *cir;
};

struct canvas
{
		float x;
		float y;
		struct canvas_node *first;
};

struct canvascircle
{
		float x;
		float y;
		struct canvascircle_node *first;
};

// --------------------------------------------------------
// FUNCTION PROTOTYPES
// --------------------------------------------------------
svg* svg_create(int width, int height);
void svg_finalize(svg* psvg);
void svg_print(svg* psvg);
void svg_save(svg* psvg, char* filepath);
void svg_free(svg* psvg);
void svg_bezier(svg *psvg, int x1, int y1);
void svg_circle(svg *psvg, int x1, int y1, int r);
void svg_line(svg* psvg, char* stroke, int strokewidth, int x1, int y1, int x2, int y2);
void svg_fill(svg* psvg, char* fill);
void svg_text(svg* psvg, int x, int y, char* fontfamily, int fontsize, char* fill, char* stroke, char* text);
#include<stdio.h>
#include<math.h>
#include<time.h>

#include "svg.h"

#define POINT 0

//--------------------------------------------------------
// FUNCTION PROTOTYPES
//--------------------------------------------------------
void read_canvas(struct canvas_node *node, svg *psvg);
int draw(struct canvas canv, char *filename);
// --------------------------------------------------------
// TYPE CONSTRUCTORS
// --------------------------------------------------------

struct point Point(double x, double y)
{
		struct point pt;
		pt.x = x;
		pt.y = y;
		return pt;
}

struct pixel Pixel(struct point ep1)
{
		struct pixel cv;
		cv.ep1 = ep1;
		return cv;
}

struct canvas Canvas(double x, double y)
{
		struct canvas c;
	
		c.x = x;
		c.y = y;
		c.first = 0;
		return c;
}


// ------------------------------------------------------
//  CANVAS READING/SVG RENDERING FUNCTIONS
// ------------------------------------------------------

int draw(struct canvas canv, char *filename)
{
		svg* psvg;
		psvg = svg_create(canv.x, canv.y);

		if (psvg == NULL) {
				fprintf(stderr, "could not store SVG meta data, malloc returned null");
				exit(1);
		}
		else{
			read_canvas(canv.first, psvg);
			
			svg_finalize(psvg);
			svg_save(psvg, filename);
			svg_free(psvg);
		}

		return 0;
}


void read_canvas(struct canvas_node *node, svg *psvg)
{	
		// Walk the canvas node list, render each pixel element

		if (node == NULL)
				return;
		
		struct canvas_node *next = node->next;
		struct pixel *ct = node->ct;

		int ep1x = (int) ct->ep1.x;
		int ep1y = (int) ct->ep1.y;

		svg_bezier(psvg, ep1x, ep1y); 
		
		read_canvas(next, psvg);

}


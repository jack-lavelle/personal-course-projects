#include<stdio.h>
#include<math.h>
#include<time.h>

#include "svg.h"

#define POINT 0

//--------------------------------------------------------
// FUNCTION PROTOTYPES
//--------------------------------------------------------
void read_canvas(struct canvas_node *node, svg *psvg);
void read_canvascircle(struct canvascircle_node *node, svg *psvg);
int draw(struct canvas canv, char *filename);
int drawcircle(struct canvascircle cancirv, char *filename);
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

struct circle Circle(struct point ep1, double r)
{
		struct circle crl;
		crl.ep1 = ep1;
		crl.r = r;
		return crl;
}

struct canvascircle CanvasCircle(double x, double y)
{
		
		struct canvascircle c;
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

int drawcircle(struct canvascircle cancirv, char *filename){
		svg* psvg;
		psvg = svg_create(cancirv.x, cancirv.y);
		if (psvg == NULL) {
				fprintf(stderr, "could not store SVG meta data, malloc returned null");
				exit(1);
		}
		else{
			read_canvascircle(cancirv.first, psvg);
			
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

void read_canvascircle(struct canvascircle_node *node, svg *psvg)
{	
		printf("4.");
		// Walk the canvas node list, render each pixel element
		if (node == NULL)
				return;
		
		struct canvascircle_node *next = node->next;
		struct circle *crl = node->cir;
		
		int ep1x = (int) crl->ep1.x;
		int ep1y = (int) crl->ep1.y;
		int r = (int) crl->r;
		printf("svgcircle(%d,%d,%d)", ep1x, ep1y, r);
		svg_circle(psvg, ep1x, ep1y, r); 
		
		read_canvascircle(next, psvg);

}


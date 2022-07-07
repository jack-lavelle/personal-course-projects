#include<stdio.h>
#include<math.h>
#include<time.h>

#include "svg.h"

#define POINT 0

//--------------------------------------------------------
// FUNCTION PROTOTYPES
//--------------------------------------------------------
void read_canvascircle(struct canvascircle_node *node, svg *psvg);
int drawcircle(struct canvascircle cancirv, char *filename);
// --------------------------------------------------------
// TYPE CONSTRUCTORS
// --------------------------------------------------------


struct circle Circle(struct point ep1, double r)
{

		FILE *fp;
		fp = fopen("./qwer.txt", "a+");
		fprintf(fp, "Entered circle.\n");
		fprintf(fp, "x:%f\n", ep1.x);
		fprintf(fp, "y:%f\n", ep1.y);
		fprintf(fp, "r:%f\n", r);
		
		struct circle crl;

		crl.B = r;

		struct point point;


		point.x = ep1.x;
		point.y = ep1.y;
		crl.A = point;

		fprintf(fp, "point.x:%f\n", point.x);
		fprintf(fp, "point.y:%f\n", point.y);
		fclose(fp);
		crl.D = point.x;
		crl.C = point.y;



		//printf("%f", ep1.x);
		//printf("%f", ep1.y);
		/*
        crl.x = ep1.x;
        crl.y = ep1.y;
		*/

		

		return crl;
		
}

struct canvascircle CanvasCircle(double x, double y)
{
		struct canvascircle c;
		c.x = x;
		c.y = y;
		c.first = 0;
		printf("Finished CanvasCircle.\n");
		return c;
}

// ------------------------------------------------------
//  CANVAS READING/SVG RENDERING FUNCTIONS
// ------------------------------------------------------

int drawcircle(struct canvascircle cancirv, char *filename){
		/*
		FILE *fp;
		fp = fopen("./zxcv.txt", "a+");
		fprintf(fp, "Entered drawcircle.\n");
		/*
		fprintf(fp, "x:%d\n", (int)crl->ep1.y);
		fprintf(fp, "y:%d\n", (int)crl->ep1.y);
		fprintf(fp, "r:%d\n", (int)crl->r);
		
		fclose(fp);
		*/

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


void read_canvascircle(struct canvascircle_node *node, svg *psvg)
{	
		// Walk the canvas node list, render each pixel element
		/*
		FILE *fp;

		fp = fopen("./asdff.txt", "a+");
		fprintf(fp, "Entered canvascirclread.\n");
		/*
		fprintf(fp, "x:%d\n", (int)crl->ep1.y);
		fprintf(fp, "y:%d\n", (int)crl->ep1.y);
		fprintf(fp, "r:%d\n", (int)crl->r);
		
		fclose(fp);
		*/

		if (node == NULL)
				return;
		
		struct canvascircle_node *next = node->next;
		struct circle *crl = node->cir;



		

		int r = (int) crl->B;
		int ep1y = (int) crl->D;
		int ep1x = (int) crl->C;

		

		printf("svgcircle(%d,%d,%d)", ep1x, ep1y, r);
		svg_circle(psvg, ep1x, ep1y, r); 
		
		read_canvascircle(next, psvg);

}


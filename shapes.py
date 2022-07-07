import math
def main():
    f = open("./tests/test-draw.seepp", "w")
    f.write("int main(){\n")
    canvas_name = "can"
    resolution = 10000
    step = 1.15
    global pixels
    pixels = []
    canvas((resolution, resolution), f, canvas_name)

    '''
    line((1, 1), (999, 500), f, canvas_name, step)
    line((1,1), (999, 750), f, canvas_name, step)
    line((1,1), (999, 250), f, canvas_name, step)
    line((1,1), (999, 2), f, canvas_name, step)

    line((1, 1), (500, 999), f, canvas_name, step)
    line((1,1), (750, 999), f, canvas_name, step)
    line((1,1), (250, 999), f, canvas_name, step)
    line((1,1), (2, 999), f, canvas_name, step)

    line((1,999), (999, 750), f, canvas_name, step)
    line((1,999), (999, 500), f, canvas_name, step)
    line((1,999), (999, 250), f, canvas_name, step)
    line((1,999), (999, 2), f, canvas_name, step)

    line((1,999), (750, 999), f, canvas_name, step)
    line((1,999), (500, 999), f, canvas_name, step)
    line((1,999), (250, 999), f, canvas_name, step)
    line((1,999), (2, 999), f, canvas_name, step)

    line((999,1), (1, 250), f, canvas_name, step)
    line((999,1), (1, 500), f, canvas_name, step)
    line((999,1), (1, 750), f, canvas_name, step)

    line((999,999), (1, 1), f, canvas_name, step)
    line((999,999), (1, 250), f, canvas_name, step)
    line((999,999), (1, 500), f, canvas_name, step)
    line((999,999), (1, 750), f, canvas_name, step)
    '''

    pt1 = (500, 100)
    pt2 = (350, 250)
    pt3 = (650, 250)

    pt4 = (400, 250)
    pt5 = (600, 250)
    pt6 = (250, 500)
    pt7 = (750, 500)

    pt8 = (400, 500)
    pt9 = (600, 500)
    pt10 = (100, 750)
    pt11 = (900, 750)

    pt12 = (400, 750)
    pt13 = (401, 900)
    pt14 = (600, 900)
    pt15 = (601, 750)

    line(pt1, pt2, f, canvas_name, step)
    line(pt2, pt3, f, canvas_name, step)
    line(pt1, pt3, f, canvas_name, step)

    line(pt4, pt6, f, canvas_name, step)
    line(pt5, pt7, f, canvas_name, step)
    line(pt6, pt7, f, canvas_name, step)

    line(pt8, pt11, f, canvas_name, step)
    line(pt9, pt11, f, canvas_name, step)
    line(pt10, pt11, f, canvas_name, step)

    line(pt12, pt13, f, canvas_name, step)
    line(pt13, pt14, f, canvas_name, step)
    line(pt14, pt15, f, canvas_name, step)
    
    

    # line((999,0), (0, 999), f, canvas_name, step)
    # line((0, 0), (999,999), f, canvas_name, step)
    draw(canvas_name, "autoshape.svg", f)
    f.write("return 0;\n}")
    f.close()

def draw(canvas, svg, f):
    dr = "draw("+canvas+", \""+svg+"\");"
    f.write(dr+'\n')

def line(point1, point2, f, canvas, step):
    if point1[0] < point2[0]:
        slope = (point2[1] - point1[1])/(point2[0] - point1[0])
        max = point2[0]
        x = point1[0]
        y = point1[0]
    else:
        slope = (point1[1] - point2[1])/(point1[0] - point2[0])
        max = point1[0]
        x = point2[0]
        y = point2[0]

    if slope == 0:
        b = point2[1]
    else:
        b = point2[1]/(slope*point2[0])
    
    
    
    while x <= max:
        if not (x,y) in pixels:
            pixels.append((x,y))
            px = "Point pt" + str(abs(int(x))) + str(abs(int(y)))+" = Point(" + str(abs(int(x))) + ".0 , " + str(abs(int(y))) + ".0);"
            pixel = "Pixel px"+ str(abs(int(x))) + str(abs(int(y)))+ " = Pixel("+"pt" + str(abs(int(x))) + str(abs(int(y)))+");"
            shoehorn = canvas + " -> append() px"+ str(abs(int(x))) + str(abs(int(y))) + ";"
            f.write(px+"\n"+pixel+"\n"+shoehorn+"\n")
        x += step
        y = x*slope + b
        
def canvas(size, f, canvas):
    asdf = "Canvas "+canvas+" = Canvas(" + str(size[0]) + ".0, " + str(size[1]) + ".0);"
    f.write(asdf+'\n')

if __name__ == '__main__':
    main()
'''
int main()
{
		Point pt1 = Point(23.0,376.0);
		Point pt2 = Point(420.0,69.0);
		Point pt3 = Point(241.0,379.0);
		Point pt4 = Point(495.0, 174.0);


		Pixel cv1 = Pixel(pt1);
		Pixel cv2 = Pixel(pt2);
		Pixel cv3 = Pixel(pt3);
		Pixel cv4 = Pixel(pt4);


		Canvas can = Canvas(500.0, 500.0);

		can -> append() cv1;
		can -> append() cv2;
		can -> append() cv3;
		can -> append() cv4;
		//can |= cv2;
		//can |= cv3;

		draw(can, "FOURRRRRpixels.svg");
		return 0;
}
'''
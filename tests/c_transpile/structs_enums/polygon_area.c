typedef struct {
    double x;
    double y;
} Point;

typedef struct {
    Point edges[6];
    int count;
} Polygon;

static Point point_make(double x, double y) {
    Point p;
    p.x = x;
    p.y = y;
    return p;
}

static void polygon_push(Polygon *poly, Point p) {
    if (poly->count < 6) {
        poly->edges[poly->count] = p;
        poly->count++;
    }
}

static double edge_dx(Point a, Point b) {
    return b.x - a.x;
}

static double edge_dy(Point a, Point b) {
    return b.y - a.y;
}

static double polygon_area(const Polygon *poly) {
    double sum = 0.0;
    for (int i = 0; i < poly->count; i++) {
        Point a = poly->edges[i];
        Point b = poly->edges[(i + 1) % poly->count];
        sum += a.x * b.y - b.x * a.y;
    }
    if (sum < 0.0) sum = -sum;
    return sum / 2.0;
}

int polygon_area_check(void) {
    Polygon poly;
    poly.count = 0;
    polygon_push(&poly, point_make(0.0, 0.0));
    polygon_push(&poly, point_make(4.0, 0.0));
    polygon_push(&poly, point_make(4.0, 3.0));
    polygon_push(&poly, point_make(0.0, 3.0));
    if (poly.count != 4) return 1;
    if (edge_dx(poly.edges[0], poly.edges[1]) != 4.0) return 2;
    if (edge_dy(poly.edges[1], poly.edges[2]) != 3.0) return 3;
    if (polygon_area(&poly) != 12.0) return 4;
    return 0;
}

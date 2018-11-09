var DATE = new Date();
var TIME = DATE.getSeconds();
var BOOST = 0;
var DIRECTION = 1;
var COLORA = '#FFF';
var COLORB = '#000';
var CANVAS = document.getElementById('virtual-paint-stirring-machine');
var CONTEXT = CANVAS.getContext('2d');
var WIDTH = window.innerWidth;
var HEIGHT = window.innerHeight;
var FRAMERATE = 100 * (1 / 60);
CANVAS.width = WIDTH;
CANVAS.height = HEIGHT;
/** Return the N unless it is less than LOWER or greater than UPPER. */
function clamp(lower, n, upper) {
    return lower < n ? Math.min(n, upper) : lower;
};
/** The sigmoid of N. */
function sig(n) {
    return 1 / (Math.exp(-n) + 1);
};
/** Draw a line from the point (X1, Y1) to (X2, Y2). */
function drawLine(context, x1, y1, x2, y2, color) {
    context.beginPath();
    context.moveTo(x1, y1);
    context.lineTo(x2, y2);
    context.strokeStyle = color;
    context.stroke();
    return undefined;
};
/** Draw a dot at the point (X, Y). */
function drawDot(context, x, y, radius, color) {
    context.beginPath();
    context.arc(x, y, radius, 0, 2 * Math.PI);
    context.fillStyle = color;
    context.fill();
    return undefined;
};
/**
 * Invert the background and current drawing colours, and
 * reverse the flow of `*TIME*'.
 */
function invertThings(event) {
    BOOST += 0.003;
    DIRECTION = -DIRECTION;
    var _js1 = COLORB;
    var _js2 = COLORA;
    COLORA = _js1;
    COLORB = _js2;
    CANVAS.style.backgroundColor = COLORA;
    return undefined;
};
/**
 * Draw a tree rooted at the point (X1, Y1) and shaped based on
 * TIME. n.b. the magic numbers are all chosen based on what looked good.
 */
function drawTree(time, offset, depth, currentX, currentY, prevX, prevY) {
    if (depth >= 1) {
        if (depth < 5) {
            drawLine(CONTEXT, currentX, currentY, prevX, prevY, COLORB);
            drawDot(CONTEXT, currentX, currentY, 1 / depth, COLORA);
        };
        return [Math.sin(time / 1.2) * Math.tan(Math.cos(time / 1.3)), Math.cos(time / 1.5) * sig(Math.sin(time / 1.7))].map(function (x) {
            return drawTree(time, offset + x, depth / 1.6, 0.5 * ((currentX + 40 * Math.cos(40 * sig(Math.cos(offset)))) + (currentX - 20 * Math.cos(time) - 20 * Math.sin(offset))), 0.5 * ((currentY + 40 * Math.cos(40 * sig(Math.sin(offset)))) + (currentY - 20 * Math.sin(time) - 20 * Math.cos(offset))), currentX, currentY);
        });
    };
};
/** Draw a tree with DRAW-TREE then recur after TIMEOUT (milliseconds). */
function drawingLoop(timeout) {
    return setTimeout(function () {
        var _ps_incr_place3 = DIRECTION * ((BOOST /= 1.01) + 1 / (1000 - TIME));
        TIME += _ps_incr_place3;
        drawTree(TIME, 0, 100, WIDTH / 2, HEIGHT / 2);
        return drawingLoop(timeout);
    }, timeout);
};
drawingLoop(FRAMERATE);
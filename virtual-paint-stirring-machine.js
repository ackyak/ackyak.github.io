var canvas = document.getElementById('virtual-paint-stirring-machine');
var context = canvas.getContext('2d');
var date = new Date();
var time = date.getSeconds();
var boost = 0;
var width = window.innerWidth;
var height = window.innerHeight;
var startX = width / 2;
var startY = height / 2;
var cursorX;
var cursorY;
var direction = 1;
var colorA = '#FFF';
var colorB = '#000';
function clamp(l, m, h) {
    return l < m ? Math.min(m, h) : Math.max(l, m);
};
function sig(n) {
    return 1 / (Math.exp(-n) + 1);
};
function line(context, x1, y1, x2, y2, color) {
    context.beginPath();
    context.moveTo(x1, y1);
    context.lineTo(x2, y2);
    context.strokeStyle = color;
    context.stroke();
    return undefined;
};
function point(context, x, y, radius, color) {
    context.beginPath();
    context.arc(x, y, radius, 0, 2 * Math.PI);
    context.fillStyle = color;
    context.fill();
    return undefined;
};
function cursorClick(event) {
    boost += 0.003;
    direction = -direction;
    cursorX = event.clientX;
    cursorY = event.clientY;
    var _js5 = colorB;
    var _js6 = colorA;
    colorA = _js5;
    colorB = _js6;
    canvas.style.backgroundColor = colorA;
    return undefined;
};
(function () {
    var drawTree = function (time, g, r, x1, y1, x2, y2) {
        if (r >= 1) {
            if (r < 5) {
                line(context, x1, y1, x2, y2, colorB);
                point(context, x1, y1, 1 / r, colorA);
            };
            return [Math.sin(time / 1.2) * Math.tan(Math.cos(time / 1.3)), Math.cos(time / 1.5) * sig(Math.sin(time / 1.7))].map(function (x) {
                return drawTree(time, g + x, r / 1.6, ((x1 + 40 * Math.cos(40 * sig(Math.cos(g)))) + (x1 - 20 * Math.cos(time) - 20 * Math.sin(g))) / 2, ((y1 + 40 * Math.cos(40 * sig(Math.sin(g)))) + (y1 - 20 * Math.sin(time) - 20 * Math.cos(g))) / 2, x1, y1);
            });
        };
    };
    var recur = function (timeout) {
        return setTimeout(function () {
            var _ps_incr_place8 = direction * ((boost /= 1.01) + 1 / (1000 - time));
            time += _ps_incr_place8;
            drawTree(time, 0, 100, startX, startY);
            return recur(timeout);
        }, timeout);
    };
    canvas.width = width;
    canvas.height = height;
    cursorX = startX;
    cursorY = startY;
    return recur(100 * (1 / 60));
})();
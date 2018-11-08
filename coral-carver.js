(function () {
    var canvas = document.getElementById('coral-thing');
    var ctx = canvas.getContext('2d');
    var widthPixels = window.innerWidth;
    var heightPixels = window.innerHeight;
    function tree(time, g, r, x1, y1, x2, y2) {
        if (r >= 1) {
            ctx.beginPath();
            ctx.moveTo(x1, y1);
            ctx.lineTo(x2, y2);
            ctx.stroke();
            ctx.arc(x1, y1, 1 / r, 0, 2 * Math.PI);
            ctx.fillStyle = '#FFF';
            ctx.fill();
            return [Math.sin(time / 1.2) * Math.tan(Math.cos(time / 1.3)), Math.cos(time / 1.5) * (1 / (Math.exp(-Math.sin(time / 1.7)) + 1))].map(function (x) {
                return tree(time, g + x, r / 1.6, ((x1 + 40 * Math.cos(40 * (1 / (Math.exp(-Math.cos(g)) + 1)))) + (x1 - 20 * Math.cos(time) - 20 * Math.sin(g))) / 2, ((y1 + 40 * Math.cos(40 * (1 / (Math.exp(-Math.sin(g)) + 1)))) + (y1 - 20 * Math.sin(time) - 20 * Math.cos(g))) / 2, x1, y1);
            });
        };
    };
    canvas.width = widthPixels;
    canvas.height = heightPixels;
    var date = new Date();
    var time = date.getSeconds();
    var ox = widthPixels / 2;
    var oy = heightPixels / 2;
    var recur = function (i) {
        return setTimeout(function () {
            var _ps_incr_place773 = 1 / (1000 - time);
            time += _ps_incr_place773;
            tree(time, 0, 100, ox, oy);
            return recur(i);
        }, i);
    };
    return recur(100 * (1 / 60));
})();
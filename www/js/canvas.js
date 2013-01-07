//
//  Canvas wrapper class
// 

function get_canvas_context(element) {
    var canvas = obj(element);
    if (canvas)
	return canvas.getContext('2d');
    return null;
}
    
    /* Canvas stuff */
    function CanvasClass(o) {
	return this;
    }

    CanvasClass.prototype.save = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.save();
    }

    CanvasClass.prototype.restore = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.restore();
    }

    CanvasClass.prototype.scale = function (canvas,x,y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.scale(x,y);
    }

    CanvasClass.prototype.rotate = function (canvas,a) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.rotate(a);
    }

    CanvasClass.prototype.translate = function (canvas,x,y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.translate(x,y);
    }

    CanvasClass.prototype.transform = function (canvas,a,b,c,d,e,f) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.transform(a,b,c,e,d,f);
    }

    CanvasClass.prototype.setTransform = function (canvas,a,b,c,d,e,f) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.setTransform(a,b,c,e,d,f);
    }


    CanvasClass.prototype.globalAlpha = function(canvas,a) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.globalAlpha = a;
    }

    CanvasClass.prototype.globalCompositeOperation = function(canvas,op) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.globalCompositeOperation = op;
    }

    CanvasClass.prototype.strokeStyle = function(canvas,style) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.strokeStyle = style;
    }

    CanvasClass.prototype.fillStyle = function(canvas,style) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.fillStyle = style;
    }

    /* Fixme: add gradient stuff */

    CanvasClass.prototype.lineWidth = function(canvas,w) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.lineWidth = w;
    }

    CanvasClass.prototype.lineCap = function(canvas,cap) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.lineCap = cap;
    }

    CanvasClass.prototype.lineJoin = function(canvas,join) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.lineJoin = join;
    }
    
    CanvasClass.prototype.miterLimit = function(canvas,lim) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.miterLimit = lim;
    }

    /* Shadow */

    CanvasClass.prototype.shadowOffsetX = function(canvas,x) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.shadowOffsetX = x;
    }

    CanvasClass.prototype.shadowOffsetY = function(canvas,y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.shadowOffsetX = Y;
    }

    CanvasClass.prototype.shadowBlur = function(canvas,b) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.shadowBlur = b;
    }

    CanvasClass.prototype.shadowColor = function(canvas,color) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.shadowColor = color;
    }

    /* rects */
    CanvasClass.prototype.clearRect = function(canvas,x,y,w,h) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.clearRect(x, y, w, h);
    }

    CanvasClass.prototype.fillRect = function(canvas,x,y,w,h) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.fillRect(x, y, w, h);
    }

    CanvasClass.prototype.strokeRect = function(canvas,x,y,w,h) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.strokeRect(x, y, w, h);
    }

    /* Path API */
    CanvasClass.prototype.beginPath = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.beginPath();
    }

    CanvasClass.prototype.closePath = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.closePath();
    }

    CanvasClass.prototype.moveTo = function(canvas,x,y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.moveTo(x,y);
    }

    CanvasClass.prototype.lineTo = function(canvas,x,y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.lineTo(x,y);
    }

    CanvasClass.prototype.quadraticCurveTo = 
    function(canvas,cp1x,cp1y,x,y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.quadraticCurveTo(cp1x,cp1y,x,y);
    }

    CanvasClass.prototype.bezierCurveTo = 
    function(canvas,cp1x, cp1y, cp2x, cp2y, x, y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y);
    }

    CanvasClass.prototype.arcTo = 
    function(canvas, x1,y1,x2,y2,r) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.arcTo(x1,y1,x2,y2,r);
    }
    
    CanvasClass.prototype.rect = function(canvas,x,y,w,h) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.rect(x, y, w, h);
    }

    CanvasClass.prototype.arc = function(canvas,x,y,r,a0,a1,acw) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.arc(x,y,r,a0,a1,acw);
    }

    CanvasClass.prototype.fill = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.fill();
    }

    CanvasClass.prototype.stroke = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.stroke();
    }

    CanvasClass.prototype.clip = function(canvas) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.clip();
    }

    CanvasClass.prototype.isPointInPath = function(canvas, x, y) {
	var ctx = get_canvas_context(canvas);
	if (ctx) return ctx.isPointInPath(canvas, x, y);
	else return false;
    }

    /* drawFocusRing */

    CanvasClass.prototype.font = function(canvas, f) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.font = f;
    }

    CanvasClass.prototype.textAlign = function(canvas, a) {    
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.textAlign = a;
    }

    CanvasClass.prototype.textBaseline = function(canvas, b) {    
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.textBaseline = b;
    }

    CanvasClass.prototype.fillText = function(canvas,text,x,y,maxWidth) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.fillText(text,x,y,maxWidth);
    }

    CanvasClass.prototype.strokeText = function(canvas,text,x,y,maxWidth) {
	var ctx = get_canvas_context(canvas);
	if (ctx) ctx.strokeText(text,x,y,maxWidth);
    }

    CanvasClass.prototype.measureText = function(canvas,text) {
	var ctx = get_canvas_context(canvas);
	if (ctx) return ctx.measureText(text);
	else return -1;
    }

    CanvasClass.prototype.drawImage2 = 
    function(canvas,image,x,y) {
	var ctx = get_canvas_context(canvas);
	var ni  = document.images[image];
	console.log("drawImage2: name=%s", image);
	console.log(ni);
	if (ctx) ctx.drawImage(ni,x,y);
    }

    CanvasClass.prototype.drawImage4 = 
    function(canvas,image,x,y,w,h) {
	var ctx = get_canvas_context(canvas);
	var ni  = document.images[image];
	console.log("drawImage: name=%s", image);
	console.log(ni);
	if (ctx) ctx.drawImage(ni,x,y,w,h);
    }

    CanvasClass.prototype.drawImage8 = 
    function(canvas,image,x1,y1,w1,h1,x2,y2,w2,h2) {
	var ctx = get_canvas_context(canvas);
	var ni  = document.images[image];
	console.log("drawImage: name=%s", image);
	console.log(ni);
	if (ctx) ctx.drawImage(ni,x1,y1,w1,h1,x2,y2,w2,h2);
    }

    function do_scroll(ctx, x,y,w,h,dx,dy,clearStyle) {
	var ax = (dx >= 0) ? dx : -dx; // abs(dx)
	var ay = (dy >= 0) ? dy : -dy; // abs(dy)
	var sx = x + ((dx >= 0) ? 0  : ax);
	var sy = y + ((dy >= 0) ? 0  : ay);
	var tx = x + ((dx > 0) ? ax : 0);
	var ty = y + ((dy > 0) ? ay : 0);
	ctx.drawImage(ctx.canvas,
		      sx,sy,w-ax,h-ay,
		      tx,ty,w-ax,h-ay);
	if (clearStyle) {
	    var cx = x + ((dx < 0) ? (w-ax-1) : 0);
	    var cy = y + ((dy < 0) ? (h-ay-1) : 0);
	    var cw = ax ? ax : w;
	    var ch = ay ? ay : h;
	    ctx.save();
	    ctx.fillStyle = clearStyle;
	    ctx.fillRect(cx,cy,cw,ch);
	    ctx.restore();
	}
    }

    function do_load_image(ctx,canvas,name,url) {
	var img = document.images[name];
	if (!img) {
	    img = new Image();
	    document.images[name] = img;
	    img.onload = function() {
		var func = obj('page').imageLoaded;
		console.log(obj('page'));
		console.log("func = %s\n", func);
		func.call(Bert.atom('loaded'),canvas.substr(6),name);
	    }
	    console.log("create image: name=%s", name);
	}
	img.src = url;
	console.log("image: set url=%d", url);
    }

    /* special hacks */
    CanvasClass.prototype.scroll = function(canvas,x,y,w,h,dx,dy,clearStyle) {
	var ctx = get_canvas_context(canvas);
	if (ctx) do_scroll(ctx, x,y,w,h,dx,dy,clearStyle);
    }

    CanvasClass.prototype.loadImage = function(canvas,name,url) {
	var ctx = get_canvas_context(canvas);
	if (ctx) do_load_image(ctx,canvas,name,url);
    }

    CanvasClass.prototype.batch = function(canvas, commands) {
	var ctx = get_canvas_context(canvas);

	if (!ctx)
	    return;
	for (i = 0; i < commands.length; i++) {
	    var argv = commands[i];
	    switch(argv[0]) {
	    case 'save':  ctx.save(); break;
	    case 'restore': ctx.restore(); break;
	    case 'scale': ctx.scale(argv[1], argv[2]); break;
	    case 'rotate': ctx.rotate(argv[1]); break;
	    case 'translate': ctx.translate(argv[1],argv[2]); break;
	    case 'transform':
		ctx.transform(argv[1],argv[2],argv[3],argv[4],argv[5]);
		break;
	    case 'setTransform':
		ctx.setTransform(argv[1],argv[2],argv[3],argv[4],argv[5]);
		break;
	    case 'globalAlpha':	ctx.globalAlpha=argv[1]; break;
	    case 'globalCopositeOperation':
		ctx.globalCopositeOperation=argv[1]; break;
	    case 'strokeStyle': ctx.strokeStyle=argv[1]; break;
	    case 'fillStyle':   ctx.fillStyle=argv[1]; break;
	    case 'lineWidth':   ctx.lineWidth=argv[1]; break;
	    case 'lineCap':     ctx.lineCap=argv[1]; break;
	    case 'lineJoin':    ctx.lineJoin=argv[1]; break;
	    case 'miterLimit':  ctx.miterLimit=argv[1]; break;
	    case 'shadowOffsetX': ctx.shadowOffsetX=argv[1]; break;
	    case 'shadowOffsetY': ctx.shadowOffsetY=argv[1]; break;
	    case 'shadowBlur':    ctx.shadowBlur=argv[1]; break;
	    case 'shadowColor':   ctx.shadowColor=argv[1]; break;
	    case 'clearRect': 
		ctx.clearRect(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'fillRect': 
		ctx.fillRect(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'strokeRect': 
		ctx.strokeRect(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'beginPath': ctx.beginPath(); break;
	    case 'closePath': ctx.closePath(); break;
	    case 'moveTo': ctx.moveTo(argv[1],argv[2]); break;
	    case 'lineTo': ctx.lineTo(argv[1],argv[2]); break;
	    case 'quadraticCurveTo':
		ctx.quadraticCurveTo(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'bezierCurveTo':
		ctx.bezierCurveTo(argv[1],argv[2],argv[3],argv[4],
				  argv[5],argv[6]);
		break;
	    case 'arcTo':
		ctx.arcTo(argv[1],argv[2],argv[3],argv[4],argv[5]);
		break;
	    case 'rect':
		ctx.rect(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'arc':
		ctx.arc(argv[1],argv[2],argv[3],argv[4],
			argv[5],argv[6]);
		break;
	    case 'fill':   ctx.fill();  break;
	    case 'stroke': ctx.stroke(); break;
	    case 'clip':   ctx.clip(); break;
	    case 'isPointInPath':
		// fixme handle return values!
		ctx.isPointInPath(argv[1],argv[2]);
		break;
	    case 'font': ctx.font = argv[1]; break;
	    case 'textAlign': ctx.textAlign = argv[1]; break;
	    case 'textBaseline': ctx.textBaseline = argv[1]; break;
	    case 'fillText':
		ctx.fillText(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'strokeText':
		ctx.strokeText(argv[1],argv[2],argv[3],argv[4]);
		break;
	    case 'measureText':
		// fixme handle return value
		ctx.measureText(argv[1]);
		break;
	    case 'drawImage':
		switch (argv.length) {
		case 4: 
		    ctx.drawImage(argv[1],argv[2],argv[3]);
		    break;
		case 6:
		    ctx.drawImage(argv[1],argv[2],argv[3],argv[4],argv[5]);
		    break;
		case 10:
		    ctx.drawImage(argv[1],argv[2],argv[3],argv[4],argv[5],
				  argv[6],argv[7],argv[8],argv[9]);
		    break;
		}
		break;

	    case 'scroll':
		do_scroll(ctx, argv[1],argv[2],argv[3],argv[4],
			  argv[5],argv[6],argv[7]);
		break;

	    case 'loadImage':
		do_load_image(ctx,canvas,argv[1],argv[2]);
		break;
	    }
	}
    }


    var Canvas = new CanvasClass();
    

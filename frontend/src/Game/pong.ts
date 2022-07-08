// class representing the Pong game
class Pong {
	private canvas: HTMLCanvasElement;
	private ctx: CanvasRenderingContext2D;
	private wall: Wall[] = [];
	private ball: Ball[] = [];
	
	constructor() {
		this.canvas = document.getElementById('canvas') as HTMLCanvasElement;
		this.ctx = this.canvas.getContext('2d') as CanvasRenderingContext2D;
		
		this.create_background();
		requestAnimationFrame(this.start.bind(this));
	}
	
	// create background for the game
	create_background(): void {
		// fill the background with black colour
		this.ctx.fillStyle = "black";
		this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
		
		// create walls
		const width = this.canvas.width;
		const height = this.canvas.height;
		const border = this.canvas.width * 0.025;
		
		this.wall.push(new Wall(width/2, border/2, width, border));
		this.wall.push(new Wall(width/2, height - border/2, width, border));
		this.wall.push(new Wall(border/2, height/2, border, height));
		for (let i = 0; i < this.wall.length; i++)
			this.wall[i].draw(this.ctx);
		
		// create the balls
		const velocity = width / 120;	// takes 2 seconds to travel across
		this.ball.push(new Ball(width/2, height/2, -velocity, velocity*2));
		for (let i = 0; i < this.ball.length; i++)
			this.ball[i].draw(this.ctx, Ball.colour);
	}
	
	// update the objects in the canvas
	update(): void {
		// check for ball-wall collision and alter direction of ball if required
		for (const w in this.wall)
		{
			const wall = this.wall[w];
			for (const b in this.ball)
			{
				const ball = this.ball[b];
				if (wall.collide(ball))
					ball.reflect(wall);
			}
		}
		
		// update the ball position and redraw
		for (const b in this.ball)
			this.ball[b].update(this.ctx);
			
		// redraw the walls
		for (const w in this.wall)
			this.wall[w].draw(this.ctx);
		
	}
	
	// start a new game
	start(): void {
		this.update();
		requestAnimationFrame(this.start.bind(this));
	}
}


class Wall {
	static colour = "white";
	
	private x: number;
	private y: number;
	width: number;
	height: number;
	
	constructor(x: number, y: number, width: number, height: number) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}
	
	// draw the wall using the context
	draw(ctx: CanvasRenderingContext2D): void {
		ctx.fillStyle = Wall.colour;
		ctx.fillRect(
			this.x - this.width/2, this.y - this.height/2,
			this.x + this.width/2, this.y + this.height/2);
	}
	
	// return true if a given ball collides with a wall, false otherwise.
	collide(ball: Ball): boolean {
		const distance = Math.min(
			Math.abs(ball.x - (this.x - this.width/2)),
			Math.abs(ball.x - (this.x + this.width/2)),
			Math.abs(ball.y - (this.y - this.height/2)),
			Math.abs(ball.y - (this.y + this.height/2))
		)
		return (distance <= Ball.radius);
	}
}


class Paddle {
	private x: number;
	private y: number;
	private width: number;
	private height: number;
	
	update() {
		return ;
	}
	
	draw() {
		return ;
	}
}


class Ball {
	static colour = "white";
	static radius = 5;
	
	x: number;
	y: number;
	vx: number;	// velocity along x
	vy: number;	// velocity along y
	
	constructor(x: number, y: number, vx:number, vy:number) {
		this.x = x;
		this.y = y;
		this.vx = vx;
		this.vy = vy;
	}
	
	// Given that a ball collides with a given wall, alter direction of ball
	reflect(wall: Wall): void {
		if (wall.height >= wall.width)
			this.vx *= -1;
		else
			this.vy *= -1;
	}
	
	/* Update the position of the ball by colouring in previous position
	 * with background colour, update x and y and drawing in current
	 * position with foreground colour. */
	update(ctx: CanvasRenderingContext2D): void {
		this.draw(ctx, "black");
		this.x += this.vx;
		this.y += this.vy;
		this.draw(ctx, Ball.colour);
	}
	
	// draw the ball with the specified colour
	draw(ctx: CanvasRenderingContext2D, colour: string): void {
		ctx.beginPath();
		ctx.arc(this.x, this.y, Ball.radius, 0, 2 * Math.PI);
		ctx.fillStyle = colour;
		ctx.fill();
		ctx.stroke();
	}
}


const pong: Pong = new Pong();

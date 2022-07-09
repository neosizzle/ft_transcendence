// class representing the Pong game
class Pong {
	private canvas: HTMLCanvasElement;
	private ctx: CanvasRenderingContext2D;
	private wall: Wall[] = [];
	private ball: Ball[] = [];
	private paddle: Paddle[] = [];
	
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
		for (const w in this.wall)
			this.wall[w].draw(this.ctx);
		
		// create paddles
		this.paddle.push(new Paddle(
				width - border*3/2, height/2, border, height*0.2, 0, -4));
		for (const p in this.paddle)
			this.paddle[p].draw(this.ctx);
		
		// create the balls
		const velocity = width / 120;	// takes 2 seconds to travel across
		this.ball.push(new Ball(width/2, height/2, 5, -velocity, 0));
		for (const b in this.ball)
			this.ball[b].draw(this.ctx);
	}
	
	// update the objects in the canvas
	update(): void {
		// check for paddle-wall collision and alter the velocity of the paddle
		for (const p in this.paddle)
		{
			const paddle = this.paddle[p];
			for (const w in this.wall)
			{
				const wall = this.wall[w];
				if (paddle.collide(wall))
					paddle.halt(wall);
			}
		}
		
		// update paddle position and redraw
		for (const p in this.paddle)
			this.paddle[p].update(this.ctx);
		
		// check for ball-wall collision and alter direction of ball if required
		for (const b in this.ball)
		{
			const ball = this.ball[b];
			for (const w in this.wall)
			{
				const wall = this.wall[w];
				if (wall.collide(ball))
					ball.reflect(wall);
			}
			
			for (const p in this.paddle)
			{
				const paddle = this.paddle[p];
				if (paddle.collide(ball))
					ball.reflect(paddle);
			}
		}
		
		// update ball position and redraw
		for (const b in this.ball)
			this.ball[b].update(this.ctx);
		
		// redraw the paddles
		for (const p in this.paddle)
			this.paddle[p].draw(this.ctx, this.paddle[p].colour);
			
		// redraw the walls
		for (const w in this.wall)
			this.wall[w].draw(this.ctx);
		
	}
	
	// start a new game
	start(): void {
		this.update();
		
		// game over if any ball is outside the canvas
		for (const b in this.ball)
		{
			if (this.ball[b].inCanvas(this.canvas) == false)
			{
				console.log("Game Over!");
				return ;
			}
		}
		
		// move to next frame if all balls are still within canvas
		requestAnimationFrame(this.start.bind(this));
	}
}


enum CollideSide {
	NONE = 0,
	LEFT = -1,
	RIGHT = 1,
	TOP = -2,
	BOTTOM = 2
}


// A generic entity with bounding box for other classes to derive from
abstract class Entity {
	x: number;	// x-coordinate of centre point
	y: number;	// y-coordinate of centre point
	width: number;	// width of bounding box
	height: number;	// height of bounding box
	vx: number;	// velocity along x direction
	vy: number;	// velocity along y direction
	colour: string;	// colour of the entity
	
	constructor(x: number, y: number, width: number, height: number,
			vx = 0, vy = 0) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		this.vx = vx;
		this.vy = vy;
		this.colour = "white";	// white by default
	}
	
	// abstract draw method to be implemented by derived classes
	abstract draw(ctx: CanvasRenderingContext2D, colour?: string): void;
	
	// if there is movement, draw object black, move to new position and redraw
	update(ctx: CanvasRenderingContext2D): void {
		if (this.vx != 0 || this.vy != 0)
			this.draw(ctx, "black");
			this.x += this.vx;
			this.y += this.vy;
			this.draw(ctx, this.colour);
	}
	
	// return the side which collides with another entity, otherwise NONE
	collide(other: Entity): CollideSide {
		// if no collision occurs, return NONE
		if (Math.abs(this.x - other.x) * 2 > this.width + other.width
			|| Math.abs(this.y - other.y) * 2 > this.height + other.height)
			return CollideSide.NONE;
		
		const dx: number = (this.width + other.width) / 2
			- Math.abs(this.x - other.x);
		const dy: number = (this.height + other.height) / 2
			- Math.abs(this.y - other.y);
		
		if (dx <= dy)
		{
			if (this.x <= other.x)
				return CollideSide.RIGHT;
			else
				return CollideSide.LEFT;
		}
		else
		{
			if (this.y <= other.y)
				return CollideSide.BOTTOM;
			else
				return CollideSide.TOP;
		}
	}
	
	// halt the movement if the object if collision happens
	halt(other: Entity): void {
		const side: CollideSide = this.collide(other);
		if (side == CollideSide.NONE)
			return;
		if (side == CollideSide.LEFT || side == CollideSide.RIGHT)
			this.vx = 0;
		else if (side == CollideSide.TOP || side == CollideSide.BOTTOM)
			this.vy = 0;
	}
	
	// reflect the direction of the object velocity if collision happens
	reflect(wall: Wall): void {
		const side: CollideSide = this.collide(wall);
		if (side == CollideSide.NONE)
			return;
		if (side == CollideSide.LEFT)
			this.vx = Math.abs(this.vx);
		else if (side == CollideSide.RIGHT)
			this.vx = -Math.abs(this.vx);
		else if (side == CollideSide.TOP)
			this.vy = Math.abs(this.vy);
		else if (side == CollideSide.BOTTOM)
			this.vy = -Math.abs(this.vy);
	}
}


// class for which rectangle-shaped entities can be derived from
class RectangleEntity extends Entity {
	// draw the wall using the context
	draw(ctx: CanvasRenderingContext2D, colour?: string): void {
		if (colour === undefined)
			ctx.fillStyle = this.colour;
		else
			ctx.fillStyle = colour;
		ctx.fillRect(
			this.x - this.width/2, this.y - this.height/2,
			this.width, this.height);
	}
}


// class for which circle-shaped entities can be derived from
class CircleEntity extends Entity {
	radius: number;
	
	constructor(x: number, y: number, radius: number, vx:number, vy:number) {
		super(x, y, radius * 2, radius * 2, vx, vy);
		this.radius = radius
	}
	
	// draw the circle entity with the specified colour
	draw(ctx: CanvasRenderingContext2D, colour?: string): void {
		if (colour === undefined)
			ctx.fillStyle = this.colour;
		else
			ctx.fillStyle = colour;
		ctx.beginPath();
		ctx.arc(this.x, this.y, this.radius, 0, 2 * Math.PI);
		ctx.fill();
		ctx.stroke();
	}
}


class Wall extends RectangleEntity {
	constructor(x: number, y: number, width: number, height: number) {
		super(x, y, width, height, 0, 0);
	}
}


class Paddle extends RectangleEntity {}


class Ball extends CircleEntity {
	// return true if ball is in canvas, false otherwise
	inCanvas(canvas: HTMLCanvasElement): boolean {
		return (0 <= this.x && this.x <= canvas.width
			&& 0 <= this.y && this.y <= canvas.height);
	}
}


const pong: Pong = new Pong();

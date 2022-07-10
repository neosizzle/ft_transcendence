// class representing the Pong game
class Pong {
	private canvas: HTMLCanvasElement;
	private ctx: CanvasRenderingContext2D;
	private wall: Wall[] = [];
	private paddle: Paddle[] = [];
	private ball: Ball[] = [];
	private entity: Entity[] = [];
	
	constructor() {
		this.canvas = document.getElementById('canvas') as HTMLCanvasElement;
		this.ctx = this.canvas.getContext('2d') as CanvasRenderingContext2D;
		
		this.reset_background();
		this.init_entities();
		requestAnimationFrame(this.start.bind(this));
	}
	
	// reset the background with the background colour
	reset_background(): void {
		this.ctx.fillStyle = "black";
		this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
	}
	
	// initialise all entities in the game
	init_entities(): void {
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
		
		// concatenate all entities into an array
		this.entity = this.entity.concat(this.wall, this.paddle, this.ball);
	}
	
	// collide all the objects in the canvas
	collide(): void {
		for (let i = 0; i < this.entity.length - 1; i++)
			for (let j = i + 1; j < this.entity.length; j++)
				this.entity[i].collide(this.entity[j]);
	}
	
	// update the objects in the canvas
	update(): void {
		// reset the canvas to the background colour
		this.reset_background();
		
		// update all the entities in the game
		for (const i in this.entity)
			this.entity[i].update(this.ctx);
	}
	
	// start a new game
	start(): void {
		this.collide();	// detect collision among all the entities in the game
		this.update();	// update velocity and redraw all entities
		
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
	abstract draw(ctx: CanvasRenderingContext2D): void;
	
	// move to new position and redraw
	update(ctx: CanvasRenderingContext2D): void {
		this.x += this.vx;
		this.y += this.vy;
		this.draw(ctx);
	}
	
	// return the side which collides with another entity, otherwise NONE
	collide(other: Entity): void {
		let side: CollideSide;
		
		// if no collision occurs, return NONE
		if (Math.abs(this.x - other.x) * 2 > this.width + other.width
			|| Math.abs(this.y - other.y) * 2 > this.height + other.height)
			return ;
		
		const dx: number = (this.width + other.width) / 2
			- Math.abs(this.x - other.x);
		const dy: number = (this.height + other.height) / 2
			- Math.abs(this.y - other.y);
		
		if (dx <= dy)
		{
			if (this.x <= other.x)
				side = CollideSide.RIGHT;
			else
				side = CollideSide.LEFT;
		}
		else
		{
			if (this.y <= other.y)
				side = CollideSide.BOTTOM;
			else
				side = CollideSide.TOP;
		}
		
		// both object reacts to collision.
		this.react(other, side);
		other.react(this, -side);
	}
	
	// function to be called when collision happens
	abstract react(other: Entity, side: CollideSide): void;
}


// class for which rectangle-shaped entities can be derived from
abstract class RectangleEntity extends Entity {
	// draw the wall using the context
	draw(ctx: CanvasRenderingContext2D): void {
		ctx.fillStyle = this.colour;
		ctx.fillRect(
			this.x - this.width/2, this.y - this.height/2,
			this.width, this.height);
	}
}


// class for which circle-shaped entities can be derived from
abstract class CircleEntity extends Entity {
	radius: number;
	
	constructor(x: number, y: number, radius: number, vx:number, vy:number) {
		super(x, y, radius * 2, radius * 2, vx, vy);
		this.radius = radius
	}
	
	// draw the circle entity with the specified colour
	draw(ctx: CanvasRenderingContext2D): void {
		ctx.fillStyle = this.colour;
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
	
	// wall will not have any reaction upon collision
	react(other: Entity, side: CollideSide): void {
		return ;
	}
}


class Paddle extends RectangleEntity {
	// paddle halts its movement if collides with a wall
	react(other: Entity, side: CollideSide): void {
		if (!(other instanceof Wall) || side == CollideSide.NONE)
			return ;
		if (side == CollideSide.LEFT || side == CollideSide.RIGHT)
			this.vx = 0;
		else if (side == CollideSide.TOP || side == CollideSide.BOTTOM)
			this.vy = 0;
	}
}


class Ball extends CircleEntity {
	// ball always reflects upon collision
	react(other: Entity, side: CollideSide): void {
		if (side == CollideSide.NONE)
			return ;
		else if (side == CollideSide.LEFT)
			this.vx = Math.abs(this.vx);
		else if (side == CollideSide.RIGHT)
			this.vx = -Math.abs(this.vx);
		else if (side == CollideSide.TOP)
			this.vy = Math.abs(this.vy);
		else if (side == CollideSide.BOTTOM)
			this.vy = -Math.abs(this.vy);
	}
	
	// return true if ball is in canvas, false otherwise
	inCanvas(canvas: HTMLCanvasElement): boolean {
		return (0 <= this.x && this.x <= canvas.width
			&& 0 <= this.y && this.y <= canvas.height);
	}
}


const pong: Pong = new Pong();

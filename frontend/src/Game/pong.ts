// class representing the Pong game
class Pong {
	// map to remember the status of a key
	static keypress: Set<KeyboardEvent["key"]> = new Set();
	static canvas: HTMLCanvasElement
		= document.getElementById('canvas') as HTMLCanvasElement;
	static ctx: CanvasRenderingContext2D
		= Pong.canvas.getContext('2d') as CanvasRenderingContext2D;
	
	private wall: Wall[] = [];
	private paddle: Paddle[] = [];
	private ball: Ball[] = [];
	private entity: Entity[] = [];
	private	isRunning: boolean;
	private player_no;
	private ball_no;
	
	constructor(player_no: number, ball_no: number) {
		this.isRunning = false;
		
		// record the number of players
		if (player_no < 0 || player_no > 4)
			throw new RangeError("Number of player must between 0 to 4.");
		this.player_no = player_no;
		
		// record the number of balls
		if (ball_no < 1)
			throw new RangeError("There must be at least 1 ball.");
		this.ball_no = ball_no;
		
		// register keydown and keyup events
		window.addEventListener("keydown", this.keydown);
		window.addEventListener("keyup", this.keyup);
		
		this.reset_background();
		this.entities_init();
		requestAnimationFrame(this.start.bind(this));
	}
	
	// method to handle keydown event
	keydown(e: KeyboardEvent) {
		Pong.keypress.add(e.key);
	}
	
	// method to handle keyup event
	keyup(e: KeyboardEvent) {
		Pong.keypress.delete(e.key);
	}
	
	// reset the background with the background colour
	reset_background(): void {
		Pong.ctx.fillStyle = "black";
		Pong.ctx.fillRect(0, 0, Pong.canvas.width, Pong.canvas.height);
	}
	
	// initialise all entities in the game
	entities_init(): void {
		// create walls
		const width = Pong.canvas.width;
		const height = Pong.canvas.height;
		const border = Pong.canvas.width * 0.025;
		
		if (this.player_no < 4)
			this.wall.push(new Wall(width/2, border/2, width, border));
		if (this.player_no < 3)
			this.wall.push(new Wall(width/2, height - border/2, width, border));
		if (this.player_no < 2)
			this.wall.push(new Wall(border/2, height/2, border, height));
		if (this.player_no < 1)
			this.wall.push(new Wall(width - border/2, height/2, border, height));
		for (const w in this.wall)
			this.wall[w].draw();
		
		// create the balls
		const vx: number = width / 120;	// take 2s to travel across
		for (let b = 0; b < this.ball_no; b++)
		{
			const y = (Math.random() - 0.5) * (height/2 - border) + height/2;
			const vy = (Math.random() / 0.5 - 1) * vx * 2 / 3;
			this.ball.push(new Ball(width/2, y, border/2, vx, vy));
			this.ball[b].draw();
		}
		
		// create paddles
		if (this.player_no >= 4)
			this.paddle.push(new Paddle(
				width/2, border*3/2, height*0.2, border, vx,
				"a", "d", "", ""));
		if (this.player_no >= 3)
			this.paddle.push(new Paddle(
				width/2, height - border*3/2, height*0.2, border, vx,
				"ArrowLeft", "ArrowRight", "", ""));
		if (this.player_no >= 2)
			this.paddle.push(new Paddle(
				border*3/2, height/2, border, height*0.2, vx,
				"", "", "w", "s"));
		if (this.player_no >= 1)
			this.paddle.push(new Paddle(
				width - border*3/2, height/2, border, height*0.2, vx,
				"", "", "ArrowUp", "ArrowDown"));
		for (const p in this.paddle)
			this.paddle[p].draw();
		
		// concatenate all entities into an array
		this.entity = this.entity.concat(this.wall, this.paddle, this.ball);
	}
	
	// clear all the entities in the game
	entities_clear(): void {
		this.wall = [];
		this.paddle = [];
		this.ball = [];
		this.entity = [];
	}
	
	// collide all the objects in the canvas
	collide(): void {
		for (let i = 0; i < this.entity.length - 1; i++)
			for (let j = i + 1; j < this.entity.length; j++)
				this.entity[i].collide(this.entity[j]);
	}
	
	// update the objects in the canvas
	update(): void {
		// start running game if space key is pressed
		if (!this.isRunning && Pong.keypress.has(' '))
			this.isRunning = true;
		
		// do nothing if game is not running
		if (!this.isRunning)
			return ;
		
		// reset the canvas to the background colour
		this.reset_background();
		
		// update all the entities in the game
		for (const i in this.entity)
			this.entity[i].update();
	}
	
	// start a new game
	start(): void {
		this.collide();	// detect collision among all the entities in the game
		this.update();	// update velocity and redraw all entities
		
		// game over if any ball is outside the canvas
		for (const b in this.ball)
		{
			if (!this.ball[b].inCanvas(Pong.canvas))
			{
				console.log("Game Over!");
				this.isRunning = false;
				
				// reset game to initial condition
				this.entities_clear();
				this.reset_background();
				this.entities_init();
				break ;
			}
		}
		
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
	abstract draw(): void;
	
	// move to new position and redraw
	update(): void {
		this.x += this.vx;
		this.y += this.vy;
		this.draw();
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
	draw(): void {
		Pong.ctx.fillStyle = this.colour;
		Pong.ctx.fillRect(
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
	draw(): void {
		Pong.ctx.fillStyle = this.colour;
		Pong.ctx.beginPath();
		Pong.ctx.arc(this.x, this.y, this.radius, 0, 2 * Math.PI);
		Pong.ctx.fill();
		Pong.ctx.stroke();
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
	dv: number;
	left: KeyboardEvent["key"];
	right: KeyboardEvent["key"];
	up: KeyboardEvent["key"];
	down: KeyboardEvent["key"];
	
	constructor(x: number, y: number, width: number, height: number, dv: number,
			left: KeyboardEvent["key"],
			right: KeyboardEvent["key"],
			up: KeyboardEvent["key"],
			down: KeyboardEvent["key"]) {
		super(x, y, width, height, 0, 0);
		this.dv = dv;
		this.left = left;
		this.right = right;
		this.up = up;
		this.down = down;
	}
	
	// change speed after updating the object
	update(): void {
		super.update();
		this.change_speed();
	}
	
	// move the paddle by changing the speed
	change_speed(): void {
		this.vx = 0;
		this.vy = 0;
		if (Pong.keypress.has(this.left))
			this.vx -= this.dv;
		if (Pong.keypress.has(this.right))
			this.vx += this.dv;
		if (Pong.keypress.has(this.up))
			this.vy -= this.dv;
		if (Pong.keypress.has(this.down))
			this.vy += this.dv;
	}
	
	// paddle halts its movement if collides with a wall
	react(other: Entity, side: CollideSide): void {
		if (side == CollideSide.NONE)
			return ;
		if ((this.vx < 0 && side == CollideSide.LEFT)
			|| (this.vy > 0 && side == CollideSide.RIGHT))
			this.vx = 0;
		else if ((this.vy < 0 && side == CollideSide.TOP)
			|| (this.vy > 0 && side == CollideSide.BOTTOM))
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


const pong: Pong = new Pong(2, 1);

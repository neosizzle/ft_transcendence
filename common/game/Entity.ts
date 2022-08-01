export type EntityState = {
	x: number,	// x-coordinate of centre point
	y: number,	// y-coordinate of centre point
	vx: number,	// velocity along x direction
	vy: number,	// velocity along y direction
}

// A generic entity with bounding box for other classes to derive from
export abstract class Entity {
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
	
	/* eslint-disable @typescript-eslint/no-empty-function */
	control(keypress: Set<KeyboardEvent["key"]>): void {}
	/* eslint-enable @typescript-eslint/no-empty-function */
	
	// set the x, y, vx, vy of the entity
	set_state(state: EntityState): void {
		this.x = state.x;
		this.y = state.y
		this.vx = state.vx;
		this.vy = state.vy;
	}
	
	get_state(): EntityState {
		return {
			x: this.x,
			y: this.y,
			vx: this.vx,
			vy: this.vy,
		};
	}
	
	// getter functions for object boundary
	get x_min(): number { return this.x - this.width / 2; }
	get x_max(): number { return this.x + this.width / 2; }
	get y_min(): number { return this.y - this.height / 2; }
	get y_max(): number { return this.y + this.height / 2; }
	
	// abstract draw method to be implemented by derived classes
	abstract draw(
		width: number, height: number, ctx: CanvasRenderingContext2D | null
		): void;
	
	// move to new position and redraw
	update(): void {
		this.x += this.vx;
		this.y += this.vy;
	}
	
	// collide this object with other object
	collide(other: Entity): void {
		// if no collision occurs
		if (Math.abs(this.x - other.x) * 2 > this.width + other.width
			|| Math.abs(this.y - other.y) * 2 > this.height + other.height)
			return ;
		
		let left: boolean = (
			other.x_min <= this.x_min && this.x_min <= other.x_max);
		let right: boolean = (
			other.x_min <= this.x_max && this.x_max <= other.x_max);
		let top: boolean = (
			other.y_min <= this.y_min && this.y_min <= other.y_max);
		let bottom: boolean = (
			other.y_min <= this.y_max && this.y_max <= other.y_max);
		
		// 'this' is wholely inside the 'other', no reaction from the other
		if (left && right && top && bottom)
		{
			this.react(other, left, right, top, bottom);
		}
		else
		{
			// if opposite faces collide, the effects cancel each other out
			if (left && right)
				left = right = false;
			if (top && bottom)
				top = bottom = false;
			this.react(other, left, right, top, bottom);
			other.react(this, right, left, bottom, top);
		}
	}
	
	// function to be called when collision happens
	abstract react(other: Entity,
		left: boolean, right: boolean, top: boolean, bottom: boolean): void;
}


// class for which rectangle-shaped entities can be derived from
abstract class RectangleEntity extends Entity {
	// draw the wall using the context
	draw(width: number, height: number, ctx: CanvasRenderingContext2D | null)
			: void {
		if (ctx === null)
			return ;
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
	draw(width: number, height: number, ctx: CanvasRenderingContext2D | null)
			: void {
		if (ctx === null)
			return ;
		ctx.fillStyle = this.colour;
		ctx.beginPath();
		ctx.arc(this.x, this.y, this.radius, 0, 2 * Math.PI);
		ctx.fill();
		ctx.stroke();
	}
}


export class Wall extends RectangleEntity {
	constructor(x: number, y: number, width: number, height: number) {
		super(x, y, width, height, 0, 0);
	}
	
	/* eslint-disable @typescript-eslint/no-unused-vars  */
	// wall will not have any reaction upon collision
	react(other: Entity,
		left: boolean, right: boolean, top: boolean, bottom: boolean): void {
		return ;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}


export class Paddle extends RectangleEntity {
	dv: number;
	boundary_width: number;
	boundary_height: number;
	left: KeyboardEvent["key"];
	right: KeyboardEvent["key"];
	up: KeyboardEvent["key"];
	down: KeyboardEvent["key"];
	
	constructor(
			x: number, y: number, width: number, height: number, dv: number,
			boundary_width: number, boundary_height: number,
			left: KeyboardEvent["key"],
			right: KeyboardEvent["key"],
			up: KeyboardEvent["key"],
			down: KeyboardEvent["key"]) {
		super(x, y, width, height, 0, 0);
		this.dv = 2 * dv;
		this.boundary_width = boundary_width,
		this.boundary_height = boundary_height,
		this.left = left;
		this.right = right;
		this.up = up;
		this.down = down;
	}
	
	// move the paddle by changing the speed
	control(keypress: Set<KeyboardEvent["key"]>): void {
		this.vx = 0;
		this.vy = 0;
		if (keypress.has(this.left))
			this.vx -= this.dv;
		if (keypress.has(this.right))
			this.vx += this.dv;
		if (keypress.has(this.up))
			this.vy -= this.dv;
		if (keypress.has(this.down))
			this.vy += this.dv;
		
		// prevent paddles from moving out of boundary
		if ((this.vx < 0 && this.x_min <= 0)
			|| (this.vx > 0 && this.x_max >= this.boundary_width))
			this.vx = 0;
		if ((this.vy < 0 && this.y_min <= 0)
			|| (this.vy > 0 && this.y_max >= this.boundary_height))
			this.vy = 0;
	}
	
	// paddle halts its movement if collides with other object
	react(other: Entity,
		left: boolean, right: boolean, top: boolean, bottom: boolean): void {
		if ((this.vx < 0 && left) || (this.vx > 0 && right))
			this.vx = 0;
		if ((this.vy < 0 && top) || (this.vy > 0 && bottom))
			this.vy = 0;
	}
}


export class Ball extends CircleEntity {
	boundary_width: number;
	boundary_height: number;
	
	constructor(x: number, y: number, radius: number, vx:number, vy:number,
			boundary_width: number, boundary_height: number) {
		super(x, y, radius, vx, vy);
		this.boundary_width = boundary_width;
		this.boundary_height = boundary_height;
	}
	
	// ball always reflects upon collision
	react(other: Entity,
		left: boolean, right: boolean, top: boolean, bottom: boolean): void {
		let speed = Math.pow(this.vx, 2) + Math.pow(this.vy, 2);
		if (other instanceof Paddle) {
			speed *= 1.05; // speed up by ~2.5 percent to make game ends faster
			
			// priority is given to left or right collision, in the event
			// of corner collision
			if (left || right)
			{
				this.vy = (this.y - other.y) / other.height * Math.sqrt(speed);
				this.vx = Math.sqrt(speed - Math.pow(this.vy, 2));
			}
			else if (top || bottom)
			{
				this.vx = (this.x - other.x) / other.width * Math.sqrt(speed);
				this.vy = Math.sqrt(speed - Math.pow(this.vx, 2));
			}
		}
		
		// ensure correct reflection direction
		if (left || right)
			this.vx = (left ? 1 : -1) * Math.abs(this.vx);
		if (top || bottom)
			this.vy = (top ? 1 : -1) * Math.abs(this.vy);
	}
	
	// return true if ball is in canvas, false otherwise
	in_canvas(): boolean {
		return (0 <= this.x && this.x <= this.boundary_width
			&& 0 <= this.y && this.y <= this.boundary_height);
	}
}

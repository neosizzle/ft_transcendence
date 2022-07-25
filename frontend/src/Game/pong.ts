// class to monitor keypress, designed as a singleton
class KeyPressMonitor {
	private static _instance: KeyPressMonitor;
	static keypress: Set<KeyboardEvent["key"]> = new Set();
	
	// since class is defined as singleton, make constructor function
	// private to prevent direct construction call
	private constructor() {
		console.log(KeyPressMonitor.keypress);
		
		// register keydown and keyup events
		window.addEventListener("keydown", KeyPressMonitor.keydown);
		window.addEventListener("keyup", KeyPressMonitor.keyup);
	}
	
	// create, if necessary, and return the singleton object
	public static get_instance(): KeyPressMonitor {
		if (!KeyPressMonitor._instance) {
			KeyPressMonitor._instance = new KeyPressMonitor();
		}
		return KeyPressMonitor._instance;
	}
	
	// add key to the set of pressed keys
	private static keydown(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.add(e.key);
	}
	
	// removed key to the set of pressed keys
	private static keyup(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.delete(e.key);
	}
	
	// expose the underlying has function of the set
	public static has(value) {
		return KeyPressMonitor.keypress.has(value);
	}
}

class Canvas {
	// map to remember the status of a key
	canvas: HTMLCanvasElement;
	ctx: CanvasRenderingContext2D;
	
	constructor() {
		this.canvas = document.getElementById('canvas') as HTMLCanvasElement;
		this.ctx = this.canvas.getContext('2d') as CanvasRenderingContext2D;
		
		// default text settings
		this.ctx.textBaseline = "middle";
		this.ctx.textAlign = "center";
		this.ctx.font = "30px Arial";
	}
}


// class representing the Pong game
class Pong {
	private canvas: HTMLCanvasElement;
	private ctx: CanvasRenderingContext2D;
	private ball: Ball;
	private entity: Entity[] = [];
	private	isRunning: boolean;
	private player_no: number;
	private scoreboard: ScoreBoard;
	
	constructor(canvas: HTMLCanvasElement, player_no: number) {
		this.canvas = canvas;
		this.ctx = this.canvas.getContext('2d') as CanvasRenderingContext2D;
		this.isRunning = false;
		
		// record the number of players
		if (player_no < 0 || player_no > 4)
			throw new RangeError("Number of player must between 0 to 4.");
		this.player_no = player_no;
		
		// create a scoreboard
		this.scoreboard = new ScoreBoard(player_no);
		
		this.reset_background();
		this.entities_init();
		for (const entity of this.entity)
			entity.draw(this.canvas);
		requestAnimationFrame(this.start.bind(this));
	}
	
	// reset the background with the background colour
	reset_background(): void {
		this.ctx.fillStyle = "black";
		this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
		this.scoreboard.draw(this.canvas);
	}
	
	// initialise all entities in the game
	entities_init(): void {
		const w = this.canvas.width;
		const h = this.canvas.height;
		const b = this.canvas.width * 0.025;
		
		// create the balls
		const s: number = w / 120;	// initial speed of ball
		const y = (Math.random() - 0.5) * (h/2 - b) + h/2;
		const vy = (Math.random() / 0.5 - 1) * s * 2 / 3;
		const vx = Math.sqrt(Math.pow(s, 2) - Math.pow(vy, 2));
		this.ball = new Ball(
			w/2, y, b/2, vx, vy, this.canvas.width, this.canvas.height);
		this.entity.push(this.ball);
		
		// create walls
		if (this.player_no < 4)  // top wall
			this.entity.push(new Wall(w/2, b/2, w, b));
		if (this.player_no < 3)  // bottom wall
			this.entity.push(new Wall(w/2, h - b/2, w, b));
		if (this.player_no < 2)  // left wall
			this.entity.push(new Wall(b/2, h/2, b, h));
		if (this.player_no < 1)  // right wall
			this.entity.push(new Wall(w - b/2, h/2, b, h));
		
		// create paddles
		if (this.player_no >= 4)  // top paddle
			this.entity.push(new Paddle(w/2, b*3/2, h*0.2, b, s,
				this.canvas.width, this.canvas.height,
				"a", "d", "", ""));
		if (this.player_no >= 3)  // bottom paddle
			this.entity.push(new Paddle(w/2, h - b*3/2, h*0.2, b, s,
				this.canvas.width, this.canvas.height,
				"ArrowLeft", "ArrowRight", "", ""));
		if (this.player_no >= 2)  // left paddle
			this.entity.push(new Paddle(b*3/2, h/2, b, h*0.2, s,
				this.canvas.width, this.canvas.height,
				"", "", "w", "s"));
		if (this.player_no >= 1)  // right paddle
			this.entity.push(new Paddle(w - b*3/2, h/2, b, h*0.2, s,
				this.canvas.width, this.canvas.height,
				"", "", "ArrowUp", "ArrowDown"));
	}
	
	// clear all the entities in the game
	entities_clear(): void {
		delete this.ball;
		this.entity = [];
	}
	
	// collide all the objects in the canvas
	collide(): void {
		for (const entity_0 of this.entity)
			for (const entity_1 of this.entity)
				if (entity_0 != entity_1)
					entity_0.collide(entity_1);
	}
	
	// update the objects in the canvas
	update(): void {
		// start running game if space key is pressed
		if (!this.isRunning && KeyPressMonitor.has(' '))
			this.isRunning = true;
		
		// do nothing if game is not running
		if (!this.isRunning)
		{
			this.ctx.fillText(
				"Press SPACE", this.canvas.width/2, this.canvas.height * 2/3);
			return ;
		}
		
		// reset the canvas to the background colour
		this.reset_background();
		
		// update all the entities in the game
		for (const entity of this.entity)
		{
			entity.update();
			entity.draw(this.canvas);
		}
	}
	
	// start a new game
	start(): void {
		this.collide();	// detect collision among all the entities in the game
		this.update();	// update velocity
		
		// game over if ball is outside the canvas
		if (!this.ball.in_canvas())
		{
			let loser: number;
			if (this.ball.x > this.canvas.width)
				loser = 0;
			else if (this.ball.x < 0)
				loser = 1;
			else if (this.ball.y > this.canvas.height)
				loser = 2;
			else if (this.ball.y < 0)
				loser = 3;
			
			this.scoreboard.add(loser);
			console.log("Score!");
			this.isRunning = false;
			
			// reset game to initial condition
			this.entities_clear();
			this.reset_background();
			this.entities_init();
			for (const entity of this.entity)
				entity.draw(this.canvas);
		}
		
		requestAnimationFrame(this.start.bind(this));
	}
}


class ScoreBoard{
	player_no: number;
	score: number[];
	
	constructor(player_no: number) {
		this.player_no = player_no;
		this.reset();
	}
	
	// add scores players depending to score location.
	add(loser: number) {
		// add 1 point for all players
		for (const i in this.score)
			++this.score[i];
		
		// subtract 1 point for loser
		--this.score[loser];
		
		// Do something if there's a winner
		const winner = this.get_winner();
		if (winner != -1) {
			console.log("Player " + (winner + 1).toString() + " has won!");
			this.reset();
		}
	}
	
	// draw in all entities
	draw(canvas: HTMLCanvasElement) {
		const ctx: CanvasRenderingContext2D = canvas.getContext('2d');
		ctx.save();
		ctx.setLineDash([10, 10]);
		ctx.fillStyle = 'lightgrey';
		ctx.strokeStyle = 'lightgrey';
		ctx.lineWidth = 5;
		if (this.player_no <= 2) {
			// draw line
			ctx.beginPath();
			ctx.moveTo(canvas.width / 2, 0);
			ctx.lineTo(canvas.width / 2, canvas.height);
			ctx.stroke();
			
			// draw scores
			if (this.player_no == 2) {
				ctx.fillText(this.score[0].toString(),
					canvas.width * 0.9, canvas.height * 0.2);
				ctx.fillText(this.score[1].toString(),
					canvas.width * 0.1, canvas.height * 0.2);
			}
		}
		else if (this.player_no <= 4) {
			// draw lines
			ctx.beginPath();
			ctx.moveTo(0, 0);
			ctx.lineTo(canvas.width, canvas.height);
			ctx.stroke();
			ctx.beginPath();
			ctx.moveTo(0, canvas.height);
			ctx.lineTo(canvas.width, 0);
			ctx.stroke();
			
			// draw scores
			ctx.fillText(this.score[0].toString(),
				canvas.width * 0.9, canvas.height * 0.2);
			ctx.fillText(this.score[1].toString(),
				canvas.width * 0.1, canvas.height * 0.8);
			ctx.fillText(this.score[2].toString(),
				canvas.width * 0.8, canvas.height * 0.9);
			if (this.player_no == 4)
				ctx.fillText(this.score[3].toString(),
					canvas.width * 0.2, canvas.height * 0.1);
		}
		ctx.restore();
	}
	
	/* Check whether any player has won. A player is deemed to have won
	 * if he scores 11 or above and are at least 2 points above all other
	 * players. Returns player number, or -1 if no winner. */
	get_winner(): number {
		for (let i = 0; i < this.score.length; ++i) {
			if (this.score[i] >= 11) {
				let j = 0;
				for (; j < this.score.length; ++j) {
					if (i != j && this.score[i] - this.score[j] < 2)
						break ;
				}
				if (j == this.player_no)
					return (i);
			}
		}
		
		return (-1);
	}
	
	// reset the scores
	reset(): void {
		this.score = [];
		for (let i = 0; i < this.player_no; ++i)
			this.score.push(0);
	}
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
	
	// getter functions for object boundary
	get x_min(): number { return this.x - this.width / 2; }
	get x_max(): number { return this.x + this.width / 2; }
	get y_min(): number { return this.y - this.height / 2; }
	get y_max(): number { return this.y + this.height / 2; }
	
	// abstract draw method to be implemented by derived classes
	abstract draw(canvas: HTMLCanvasElement): void;
	
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
	draw(canvas: HTMLCanvasElement): void {
		const ctx: CanvasRenderingContext2D = canvas.getContext('2d');
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
	draw(canvas: HTMLCanvasElement): void {
		const ctx: CanvasRenderingContext2D = canvas.getContext('2d');
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
	react(other: Entity,
		left: boolean, right: boolean, top: boolean, bottom: boolean): void {
		return ;
	}
}


class Paddle extends RectangleEntity {
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
	
	// change speed after updating the object
	update(): void {
		super.update();
		this.change_speed();
	}
	
	// move the paddle by changing the speed
	change_speed(): void {
		this.vx = 0;
		this.vy = 0;
		if (KeyPressMonitor.has(this.left))
			this.vx -= this.dv;
		if (KeyPressMonitor.has(this.right))
			this.vx += this.dv;
		if (KeyPressMonitor.has(this.up))
			this.vy -= this.dv;
		if (KeyPressMonitor.has(this.down))
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


class Ball extends CircleEntity {
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

const keypress: KeyPressMonitor = KeyPressMonitor.get_instance();
const canvas: Canvas = new Canvas();
const pong: Pong = new Pong(canvas.canvas, 2);

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


// class representing the Pong game
class Pong {
	// map to remember the status of a key
	static canvas: HTMLCanvasElement
		= document.getElementById('canvas') as HTMLCanvasElement;
	static ctx: CanvasRenderingContext2D
		= Pong.canvas.getContext('2d') as CanvasRenderingContext2D;
	
	private wall: Wall[] = [];
	private paddle: Paddle[] = [];
	private ball: Ball[] = [];
	private entity: Entity[] = [];
	private	isRunning: boolean;
	private player_no: number;
	private ball_no: number;
	private scoreboard: ScoreBoard;
	
	constructor(player_no: number, ball_no: number) {
		// default text settings
		Pong.ctx.textBaseline = "middle";
		Pong.ctx.textAlign = "center";
		Pong.ctx.font = "30px Arial";
		
		this.isRunning = false;
		
		// record the number of players
		if (player_no < 0 || player_no > 4)
			throw new RangeError("Number of player must between 0 to 4.");
		this.player_no = player_no;
		
		// record the number of balls
		if (ball_no < 1)
			throw new RangeError("There must be at least 1 ball.");
		this.ball_no = ball_no;
		
		// create a scoreboard
		this.scoreboard = new ScoreBoard(player_no);
		
		this.reset_background();
		this.entities_init();
		requestAnimationFrame(this.start.bind(this));
	}
	
	// reset the background with the background colour
	reset_background(): void {
		Pong.ctx.fillStyle = "black";
		Pong.ctx.fillRect(0, 0, Pong.canvas.width, Pong.canvas.height);
		this.scoreboard.draw();
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
		for (const w of this.wall)
			w.draw();
		
		// create the balls
		const speed: number = width / 120;	// initial speed of ball
		for (let b = 0; b < this.ball_no; ++b)
		{
			const y = (Math.random() - 0.5) * (height/2 - border) + height/2;
			const vy = (Math.random() / 0.5 - 1) * speed * 2 / 3;
			const vx = Math.sqrt(Math.pow(speed, 2) - Math.pow(vy, 2));
			const ball = new Ball(width/2, y, border/2, vx, vy);
			this.ball.push(ball);
			ball.draw();
		}
		
		// create paddles
		if (this.player_no >= 4)
			this.paddle.push(new Paddle(
				width/2, border*3/2, height*0.2, border, speed,
				"a", "d", "", ""));
		if (this.player_no >= 3)
			this.paddle.push(new Paddle(
				width/2, height - border*3/2, height*0.2, border, speed,
				"ArrowLeft", "ArrowRight", "", ""));
		if (this.player_no >= 2)
			this.paddle.push(new Paddle(
				border*3/2, height/2, border, height*0.2, speed,
				"", "", "w", "s"));
		if (this.player_no >= 1)
			this.paddle.push(new Paddle(
				width - border*3/2, height/2, border, height*0.2, speed,
				"", "", "ArrowUp", "ArrowDown"));
		for (const p of this.paddle)
			p.draw();
		
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
			Pong.ctx.fillText(
				"Press SPACE", Pong.canvas.width/2, Pong.canvas.height * 2/3);
			return ;
		}
		
		// reset the canvas to the background colour
		this.reset_background();
		
		// update all the entities in the game
		for (const entity of this.entity)
			entity.update();
	}
	
	// start a new game
	start(): void {
		this.collide();	// detect collision among all the entities in the game
		this.update();	// update velocity and redraw all entities
		
		// game over if any ball is outside the canvas
		for (const ball of this.ball)
		{
			if (!ball.in_canvas())
			{
				this.scoreboard.add(ball);
				console.log("Score!");
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


class ScoreBoard{
	player_no: number;
	score: number[];
	
	constructor(player_no: number) {
		this.player_no = player_no;
		this.reset();
	}
	
	// add scores players depending to score location.
	add(ball: Ball) {
		// add 1 point for all players
		for (const i in this.score)
			++this.score[i];
		
		// subtract 1 point for loser
		if (ball.x > Pong.canvas.width)
			--this.score[0];
		if (ball.x < 0)
			--this.score[1];
		if (ball.y > Pong.canvas.height)
			--this.score[2];
		if (ball.y < 0)
			--this.score[3];
		
		// Do something if there's a winner
		const winner = this.get_winner();
		if (winner != -1) {
			console.log("Player " + (winner + 1).toString() + " has won!");
			this.reset();
		}
	}
	
	// draw in all entities
	draw() {
		Pong.ctx.save();
		Pong.ctx.setLineDash([10, 10]);
		Pong.ctx.fillStyle = 'lightgrey';
		Pong.ctx.strokeStyle = 'lightgrey';
		Pong.ctx.lineWidth = 5;
		if (this.player_no <= 2) {
			// draw line
			Pong.ctx.beginPath();
			Pong.ctx.moveTo(Pong.canvas.width / 2, 0);
			Pong.ctx.lineTo(Pong.canvas.width / 2, Pong.canvas.height);
			Pong.ctx.stroke();
			
			// draw scores
			if (this.player_no == 2) {
				Pong.ctx.fillText(this.score[0].toString(),
					Pong.canvas.width * 0.9, Pong.canvas.height * 0.2);
				Pong.ctx.fillText(this.score[1].toString(),
					Pong.canvas.width * 0.1, Pong.canvas.height * 0.2);
			}
		}
		else if (this.player_no <= 4) {
			// draw lines
			Pong.ctx.beginPath();
			Pong.ctx.moveTo(0, 0);
			Pong.ctx.lineTo(Pong.canvas.width, Pong.canvas.height);
			Pong.ctx.stroke();
			Pong.ctx.beginPath();
			Pong.ctx.moveTo(0, Pong.canvas.height);
			Pong.ctx.lineTo(Pong.canvas.width, 0);
			Pong.ctx.stroke();
			
			// draw scores
			Pong.ctx.fillText(this.score[0].toString(),
				Pong.canvas.width * 0.9, Pong.canvas.height * 0.2);
			Pong.ctx.fillText(this.score[1].toString(),
				Pong.canvas.width * 0.1, Pong.canvas.height * 0.8);
			Pong.ctx.fillText(this.score[2].toString(),
				Pong.canvas.width * 0.8, Pong.canvas.height * 0.9);
			if (this.player_no == 4)
				Pong.ctx.fillText(this.score[3].toString(),
					Pong.canvas.width * 0.2, Pong.canvas.height * 0.1);
		}
		Pong.ctx.restore();
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
	abstract draw(): void;
	
	// move to new position and redraw
	update(): void {
		this.x += this.vx;
		this.y += this.vy;
		this.draw();
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
	react(other: Entity,
		left: boolean, right: boolean, top: boolean, bottom: boolean): void {
		return ;
	}
}


class Paddle extends RectangleEntity {
	dv: number;
	left: KeyboardEvent["key"];
	right: KeyboardEvent["key"];
	up: KeyboardEvent["key"];
	down: KeyboardEvent["key"];
	
	constructor(
			x: number, y: number, width: number, height: number, dv: number,
			left: KeyboardEvent["key"],
			right: KeyboardEvent["key"],
			up: KeyboardEvent["key"],
			down: KeyboardEvent["key"]) {
		super(x, y, width, height, 0, 0);
		this.dv = 2 * dv;
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
			|| (this.vx > 0 && this.x_max >= Pong.canvas.width))
			this.vx = 0;
		if ((this.vy < 0 && this.y_min <= 0)
			|| (this.vy > 0 && this.y_max >= Pong.canvas.height))
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
		return (0 <= this.x && this.x <= Pong.canvas.width
			&& 0 <= this.y && this.y <= Pong.canvas.height);
	}
}

const keypress: KeyPressMonitor = KeyPressMonitor.get_instance();
const pong: Pong = new Pong(2, 1);

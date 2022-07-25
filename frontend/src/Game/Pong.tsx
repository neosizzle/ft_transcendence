import { KeyPressMonitor } from "./KeyPressMonitor";
import { Entity, Wall, Paddle, Ball } from "./Entity"


// class representing the Pong game
export class Pong {
	// map to remember the status of a key
	static canvas: HTMLCanvasElement
		= document.getElementById('canvas') as HTMLCanvasElement;
	static ctx: CanvasRenderingContext2D
		= Pong.canvas.getContext('2d') as CanvasRenderingContext2D;
	
	private ball: Ball | undefined;
	private entity: Entity[] = [];
	private	isRunning: boolean;
	private player_no: number;
	private scoreboard: ScoreBoard;
	
	constructor(player_no: number) {
		// default text settings
		Pong.ctx.textBaseline = "middle";
		Pong.ctx.textAlign = "center";
		Pong.ctx.font = "30px Arial";
		
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
			entity.draw(Pong.canvas);
		requestAnimationFrame(this.start.bind(this));
	}
	
	// reset the background with the background colour
	reset_background(): void {
		Pong.ctx.fillStyle = "black";
		Pong.ctx.fillRect(0, 0, Pong.canvas.width, Pong.canvas.height);
		this.scoreboard.draw(Pong.canvas);
	}
	
	// initialise all entities in the game
	entities_init(): void {
		const w = Pong.canvas.width;
		const h = Pong.canvas.height;
		const b = Pong.canvas.width * 0.025;
		
		// create the balls
		const s: number = w / 120;	// initial speed of ball
		const y = (Math.random() - 0.5) * (h/2 - b) + h/2;
		const vy = (Math.random() / 0.5 - 1) * s * 2 / 3;
		const vx = Math.sqrt(Math.pow(s, 2) - Math.pow(vy, 2));
		this.ball = new Ball(
			w/2, y, b/2, vx, vy, Pong.canvas.width, Pong.canvas.height);
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
				Pong.canvas.width, Pong.canvas.height,
				"a", "d", "", ""));
		if (this.player_no >= 3)  // bottom paddle
			this.entity.push(new Paddle(w/2, h - b*3/2, h*0.2, b, s,
				Pong.canvas.width, Pong.canvas.height,
				"ArrowLeft", "ArrowRight", "", ""));
		if (this.player_no >= 2)  // left paddle
			this.entity.push(new Paddle(b*3/2, h/2, b, h*0.2, s,
				Pong.canvas.width, Pong.canvas.height,
				"", "", "w", "s"));
		if (this.player_no >= 1)  // right paddle
			this.entity.push(new Paddle(w - b*3/2, h/2, b, h*0.2, s,
				Pong.canvas.width, Pong.canvas.height,
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
			Pong.ctx.fillText(
				"Press SPACE", Pong.canvas.width/2, Pong.canvas.height * 2/3);
			return ;
		}
		
		// reset the canvas to the background colour
		this.reset_background();
		
		// update all the entities in the game
		for (const entity of this.entity)
		{
			entity.update();
			entity.draw(Pong.canvas);
		}
	}
	
	// start a new game
	start(): void {
		this.collide();	// detect collision among all the entities in the game
		this.update();	// update velocity
		
		// game over if ball is outside the canvas
		if (this.ball !== undefined && this.ball.in_canvas())
		{
			let loser = -1;
			if (this.ball.x > Pong.canvas.width)
				loser = 0;
			else if (this.ball.x < 0)
				loser = 1;
			else if (this.ball.y > Pong.canvas.height)
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
				entity.draw(Pong.canvas);
		}
		
		requestAnimationFrame(this.start.bind(this));
	}
}


class ScoreBoard{
	player_no: number;
	score: number[];
	
	constructor(player_no: number) {
		this.player_no = player_no;
		this.score = [];
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
		const ctx: CanvasRenderingContext2D
			= canvas.getContext('2d') as CanvasRenderingContext2D;
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

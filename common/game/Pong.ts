import { EntityState, Entity, Wall, Paddle, Ball } from "./Entity"


type GameState = {
	isRunning: boolean;	// true if game is running, false otherwise
	score: number[];	// current running score of the game
	entity: EntityState[];	// array of entity states
}

interface GameInterface {
	entity: Entity[];	// stores all the game entitities
	update(): void;	// update the state of all entities in the game
	set_state(state: GameState): void;	// set the state of the game
	get_state(): GameState;	// get the state of the game
}

// class representing the Pong game
export default class Pong implements GameInterface {
	private width;
	private height;
	private ball: Ball | undefined;
	private player_no: number;
	private scoreboard: ScoreBoard;
	
	entity: Entity[] = [];
	isRunning = false;
	
	constructor(width: number, height: number, player_no: number) {
		this.width = width;
		this.height = height;
		
		// record the number of players
		if (player_no < 0 || player_no > 4)
			throw new RangeError("Number of player must between 0 to 4.");
		this.player_no = player_no;
		
		// create a scoreboard
		this.scoreboard = new ScoreBoard(player_no);
		
		// initialise entities of the game
		this.entities_init();
	}
	
	set_state(state: GameState) {
		this.isRunning = state.isRunning;
		this.scoreboard.set_state(state.score);
		for (const i in state.entity)
			this.entity[i].set_state(state.entity[i]);
	}
	
	get_state(): GameState {
		const entity_state: EntityState[] = [];
		for (const i of this.entity)
			entity_state.push(i.get_state());
		
		return {
			isRunning: this.isRunning,
			score: this.scoreboard.get_state(),
			entity: entity_state,
		};
	}
	
	// initialise all entities in the game
	entities_init(): void {
		const w = this.width;
		const h = this.height;
		const b = this.width * 0.025;	// border thickness
		
		// create the balls
		const s: number = w / 120;	// initial speed of ball
		let vy = (Math.random() / 0.5 - 1) * s * 2 / 3;
		let vx = Math.sqrt(Math.pow(s, 2) - Math.pow(vy, 2));
		
		// aim ball towards last loser, or player 0 initially
		if (this.player_no <= 2) {
			if (this.scoreboard.last_loser == 1)
				vx *= -1;
		}
		else {
			[vx, vy] = [vy, vx];
			if (this.scoreboard.last_loser == 3)
				vy = -Math.abs(vy);
		}
		
		this.ball = new Ball(w/2, h/2, b/2, vx, vy, w, h);
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
			this.entity.push(new Paddle(w/2, b*3/2, h*0.2, b, s, w, h,
				"a", "d", "", ""));
		if (this.player_no >= 3)  // bottom paddle
			this.entity.push(new Paddle(w/2, h - b*3/2, h*0.2, b, s, w, h,
				"ArrowLeft", "ArrowRight", "", ""));
		if (this.player_no >= 2)  // left paddle
			this.entity.push(new Paddle(b*3/2, h/2, b, h*0.2, s, w, h,
				"", "", "w", "s"));
		if (this.player_no >= 1)  // right paddle
			this.entity.push(new Paddle(w - b*3/2, h/2, b, h*0.2, s, w, h,
				"", "", "ArrowUp", "ArrowDown"));
	}
	
	// clear all the entities in the game
	entities_clear(): void {
		delete this.ball;
		this.entity = [];
	}
	
	start(): void {
		this.isRunning = true;
	}
	
	// draw all the elements on screen using context 'ctx'
	draw(ctx: CanvasRenderingContext2D | null): void {
		// do nothing if there's no valid context
		if (ctx === null)
			return ;
		
		// reset the canvas an empty black canvas
		ctx.fillStyle = "black";
		ctx.fillRect(0, 0, this.width, this.height);
		
		// if game is not running, show the "Press SPACE" text
		if (!this.isRunning)
		{
			ctx.fillStyle = "white";
			ctx.fillText("Press SPACE", this.width * 0.525, this.height * 2/3);
		}
		
		// draw the score board
		this.scoreboard.draw(this.width, this.height, ctx);
		
		// draw all the entities in the game
		for (const entity of this.entity)
			entity.draw(this.width, this.height, ctx);
	}
	
	// update all the entities in the game, taking account collision
	update(): void {
		// nothing to update if game is not running
		if (!this.isRunning)
			return ;
		
		// collide all permutation of entities in the game
		for (const entity_0 of this.entity)
			for (const entity_1 of this.entity)
				if (entity_0 != entity_1)
					entity_0.collide(entity_1);
		
		// update all the entities to their new position
		for (const entity of this.entity)
		entity.update();

		// detect out-of-bound ball
		this.score()
	}
	
	// detect if ball is out of bound. If so, update the score and set up
	// for for next round
	score(): void {
		if (this.ball === undefined || this.ball.in_canvas())
			return ;
		
		// game over if ball is outside the canvas
		let loser: number;
		if (this.ball.x > this.width)
			loser = 0;
		else if (this.ball.x < 0)
			loser = 1;
		else if (this.ball.y > this.height)
			loser = 2;
		else if (this.ball.y < 0)
			loser = 3;
		else
			return ;
		
		this.scoreboard.add(loser);	// update the score
		console.log("Score!");
		this.isRunning = false;	// stop the current round
		
		// reset game to initial condition
		this.entities_clear();
		this.entities_init();
	}
}


class ScoreBoard{
	player_no: number;
	last_loser = 0;
	score: number[] = [];
	
	constructor(player_no: number) {
		this.player_no = player_no;
		this.reset();
	}
	
	set_state(score: number[]): void {
		this.score = score;
	}
	
	get_state(): number[] {
		return this.score;
	}
	
	// add scores players depending to score location.
	add(loser: number) {
		// add 1 point for all players
		for (const i in this.score)
			++this.score[i];
		
		// subtract 1 point for loser
		--this.score[loser];
		this.last_loser = loser;
		
		// Do something if there's a winner
		const winner = this.get_winner();
		if (winner != -1) {
			console.log("Player " + (winner + 1).toString() + " has won!");
			this.reset();
		}
	}
	
	// draw in all entities
	draw(width: number, height: number, ctx: CanvasRenderingContext2D | null) {
		if (ctx === null)
			return ;
		ctx.save();
		ctx.setLineDash([10, 10]);
		ctx.fillStyle = 'lightgrey';
		ctx.strokeStyle = 'lightgrey';
		ctx.lineWidth = 5;
		if (this.player_no <= 2) {
			// draw line
			ctx.beginPath();
			ctx.moveTo(width / 2, 0);
			ctx.lineTo(width / 2, height);
			ctx.stroke();
			
			// draw scores
			if (this.player_no == 2) {
				ctx.fillText(this.score[0].toString(),
					width * 0.9, height * 0.2);
				ctx.fillText(this.score[1].toString(),
					width * 0.1, height * 0.2);
			}
		}
		else if (this.player_no <= 4) {
			// draw lines
			ctx.beginPath();
			ctx.moveTo(0, 0);
			ctx.lineTo(width, height);
			ctx.stroke();
			ctx.beginPath();
			ctx.moveTo(0, height);
			ctx.lineTo(width, 0);
			ctx.stroke();
			
			// draw scores
			ctx.fillText(this.score[0].toString(),
				width * 0.9, height * 0.2);
			ctx.fillText(this.score[1].toString(),
				width * 0.1, height * 0.8);
			ctx.fillText(this.score[2].toString(),
				width * 0.8, height * 0.9);
			if (this.player_no == 4)
				ctx.fillText(this.score[3].toString(),
					width * 0.2, height * 0.1);
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
		this.last_loser = 0;
	}
}

import { ControlKeys, EntityState, Entity, Wall, Paddle, Ball } from "./Entity"

let DISP_SCALE: number;
if (typeof window == 'undefined')
	DISP_SCALE = 1;
else
	DISP_SCALE = Math.min(window.innerWidth, window.innerHeight) / 400;

export type GameState = {
	isRunning: boolean[];	// true if game is running, false otherwise
	score: number[];	// current running score of the game
	entity: EntityState[];	// array of entity states
}

export interface GameInterface {
	control_keys: ControlKeys[];
	entity: Entity[];	// stores all the game entitities
	type: boolean;	// true if game is customised
	control(keypress: Set<KeyboardEvent["key"]>): void;	// handles key presses
	set_player(n: number): void;	// record the current player number
	unset_player(n: number): void;	// remove player number from record
	start(n?: number): void;	// start a new round of game
	reset(reset_score: boolean): void	// reset game as a totally new game
	update(): void;	// update the state of all entities in the game
	draw(ctx: CanvasRenderingContext2D | null): void;	// draw game onto canvas
	set_state(latency: number, state: GameState): void;	// set the state of the game
	get_state(): GameState;	// get the state of the game
	set_type(type: boolean): void;	// set game type
	updateClient?: () => void;	// callback function to update game state to clients
	onGameEnd?: (winner?: number) => void;	// callback function to be run on game end
}

// class representing the Pong game
export default class Pong implements GameInterface {
	control_keys: ControlKeys[] = [
		{up: "w", down: "s"},						// player 0
		{up: "ArrowUp", down: "ArrowDown"},			// player 1
		{left: "a", right: "d"},					// player 2
		{left: "ArrowLeft", right: "ArrowRight"},	// player 3
	]
	private player: Set<number> = new Set<number>();
	private width: number;
	private height: number;
	private ball: Ball | undefined;
	private scoreboard: ScoreBoard;
	private then?: number;
	
	entity: Entity[] = [];
	isRunning: boolean[];
	type = false;
	updateClient?: () => void;
	onGameEnd?: () => void;
	
	constructor(width: number, height: number,
			updateClient?: () => void, onGameEnd?: () => void) {
		this.width = width;
		this.height = height;
		
		this.updateClient = updateClient;
		this.onGameEnd = onGameEnd;
		
		// create a scoreboard
		this.scoreboard = new ScoreBoard(onGameEnd);
		
		this.isRunning = [];
		for (let i = 0; i < 2; ++i)
			this.isRunning.push(false);
		
		// initialise entities of the game
		this.entities_init(updateClient);
	}
	
	// set which player is controlling the game
	set_player(player: number): void {
		this.player.add(player);
	}
	
	// remove player from controlling the game
	unset_player(player: number): void {
		this.player.delete(player);
	}
	
	// redirects the key events to all entities in the game
	control(keypress: Set<KeyboardEvent["key"]>): void {
		if (keypress.has(' '))
			this.start();
		if (typeof window === 'undefined') {
			for (const ent of this.entity)
				ent.control(keypress);
			if (this.updateClient != null)
				this.updateClient();
		} else {
			if (this.player.size == 0)
				return ;
			let allowed_keys: string[] = [];
			this.player.forEach((n) => {
				allowed_keys = allowed_keys.concat(
					Object.values(this.control_keys[n])
				);
			});
			const filtered_keypress: Set<KeyboardEvent["key"]> = new Set(
				allowed_keys.filter(elem => keypress.has(elem)));
			for (const ent of this.entity)
				ent.control(filtered_keypress);
		}
	}
	
	// set a state of the whole game
	set_state(latency: number, state: GameState) {
		// console.log('setting game state');
		this.isRunning = state.isRunning;
		this.scoreboard.set_state(state.score);
		for (const i in state.entity)
			this.entity[i]?.set_state(latency, state.entity[i], DISP_SCALE);
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
	
	// set game type. True for customised game, false for original
	set_type(type: boolean): void {
		// no change, do nothing.
		if (this.type == type)
			return ;
		else if (type == true) {
			// add 2 custom walls
			this.custom_wall();
		} else {
			// remove 2 walls, i.e. the last 2 items
			this.entity.splice(this.entity.length - 2, 2);
		}
		this.type = type;
	}
	
	// add 2 custom walls to the game
	custom_wall(): void {
		const w = this.width;
		const h = this.height;
		const b = this.width * 0.025;	// border thickness
		this.entity.push(new Wall(w/2, 0.3*h, b, 0.2*h));	// top
		this.entity.push(new Wall(w/2, 0.7*h, b, 0.2*h));	// bottom
	}
	
	// return whether all players have started the game
	private all_players_ready(): boolean {
		return this.isRunning.reduce((a, b) => a && b, true);
	}
	
	// initialise all entities in the game
	entities_init(updateClient?: () => void): void {
		const w = this.width;
		const h = this.height;
		const b = this.width * 0.025;	// border thickness
		
		// create the balls
		const s: number = w / 120;	// initial speed of ball
		const vy = (Math.random() / 0.5 - 1) * s * 2 / 3;
		let vx = Math.sqrt(Math.pow(s, 2) - Math.pow(vy, 2));
		
		// aim ball towards last loser, or player 0 initially
		if (this.scoreboard.last_loser == 0)
			vx *= -1;
		
		this.ball = new Ball(w/2, h/2, b/2, vx, vy, w, h, updateClient);
		this.entity.push(this.ball);
		
		// create walls
		this.entity.push(new Wall(w/2, b/2, w, b));	// top wall
		this.entity.push(new Wall(w/2, h - b/2, w, b));	// bottom wall
		
		// create paddles
		this.entity.push(new Paddle(b*3/2, h/2, b, h*0.2, s, w, h,
				this.control_keys[0]));	// left paddle
		this.entity.push(new Paddle(w - b*3/2, h/2, b, h*0.2, s, w, h,
			this.control_keys[1]));	// right paddle
			
		// custom wall if true
		if (this.type)
			this.custom_wall();
	}
	
	// clear all the entities in the game
	entities_clear(): void {
		delete this.ball;
		this.entity = [];
	}
	
	// if n is given, set player 'n' as ready; else set all recognised players
	// as ready.
	start(n?: number): void {
		if (typeof n === 'undefined')
			this.player.forEach((n) => { this.isRunning[n] = true; });
		else {
			this.isRunning[n] = true;
			// console.log(`Player ${n} is ready`);
		}
	}
	
	// reset game into a new round. Score is reset if reset_score is true.
	reset(reset_score=false): void {
		if (reset_score) {
			this.scoreboard.reset();
			this.type = false;
		}
		
		for (let n = 0; n < this.isRunning.length; ++n)
			this.isRunning[n] = false;
		
		// initialise entities of the game
		this.entities_clear()
		this.entities_init(this.updateClient);
		this.then = undefined;
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
		ctx.fillStyle = "lightgrey";
		for (let i = 0; i < this.isRunning.length; ++i)
		{
			const x = this.width * (0.25 + 0.5*i);
			const y = this.height * 0.5
			if (this.isRunning[i] == false) {
				ctx.fillText("Press", x, y - Math.floor(30 * DISP_SCALE));
				ctx.fillText("SPACE", x, y);
				let text: string;
				if (i == 0)
					text = "Keys: W/S";
				else
					text = "Keys: ↑/↓";
				ctx.fillText(text, x, y + Math.floor(30 * DISP_SCALE));
			}
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
		if (this.all_players_ready() == false)
			return ;
		
		// calculate how many frames needs to be updated
		const now = new Date().getTime();
		const interval = 1000 / 60;	// target 60 Hz
		let frame: number;
		if (this.then === undefined) {
			frame = 1;
			this.then = now;
		} else {
			const elapsed: number = now - this.then;
			frame = Math.floor(elapsed / interval);
			if (frame <= 0)
				return ;
			this.then += frame * interval;
		}
		
		// run update once per required frame
		while (frame-- > 0) {
			// collide all permutation of entities in the game
			for (const entity_0 of this.entity)
				for (const entity_1 of this.entity)
					if (entity_0 != entity_1)
						entity_0.collide(entity_1);
			
			// update all the entities to their new position
			for (const entity of this.entity)
				entity.update();
		}

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
		if (this.ball.x < 0)
			loser = 0;
		else if (this.ball.x > this.width)
			loser = 1;
		else
			return ;
		
		this.scoreboard.add(loser);	// update the score
		// console.log(`Player ${loser + 1} lost!`);
		// stop the current round
		this.reset(false);
	}
}


class ScoreBoard{
	onGameEnd?: (winner?: number) => void;
	last_loser = 0;
	score: number[] = [];
	
	constructor(onGameEnd?: () => void) {
		this.reset();
		this.onGameEnd = onGameEnd;
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
			// console.log("Player " + (winner + 1).toString() + " has won!");
			if (this.onGameEnd !== undefined)
				this.onGameEnd(winner);
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
		ctx.lineWidth = 3;
		
		// draw line
		ctx.beginPath();
		ctx.moveTo(width / 2, 15);
		ctx.lineTo(width / 2, height - 10);
		ctx.stroke();
		
		// draw scores
		ctx.fillText(this.score[0].toString(), width * 0.1, height * 0.2);
		ctx.fillText(this.score[1].toString(), width * 0.9, height * 0.2);
		
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
				if (j == 2)
					return (i);
			}
		}
		
		return (-1);
	}
	
	// reset the scores
	reset(): void {
		this.score = [];
		for (let i = 0; i < 2; ++i)
			this.score.push(0);
		this.last_loser = 0;
	}
}

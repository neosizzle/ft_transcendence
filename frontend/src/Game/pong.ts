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
		this.paddle.push(
			new Paddle(width - border*3/2, height/2, border, height*0.2));
		for (const p in this.paddle)
			this.paddle[p].draw(this.ctx);
		
		// create the balls
		const velocity = width / 120;	// takes 2 seconds to travel across
		this.ball.push(new Ball(width/2, height/2, -velocity, 0));
		for (const b in this.ball)
			this.ball[b].draw(this.ctx, Ball.colour);
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
				if (paddle.collide_wall(wall))
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
				if (wall.collide_ball(ball))
					ball.reflect(wall);
			}
			
			for (const p in this.paddle)
			{
				const paddle = this.paddle[p];
				if (paddle.collide_ball(ball))
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


class Wall {
	colour: string;
	x: number;
	y: number;
	width: number;
	height: number;
	
	constructor(x: number, y: number, width: number, height: number) {
		this.colour = "white";
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}
	
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
	
	// return true if a given ball collides with a wall, false otherwise.
	collide_ball(ball: Ball): boolean {
		const wall_top: number = this.y - this.height/2;
		const wall_bot: number = this.y + this.height/2;
		const wall_left: number = this.x - this.width/2;
		const wall_right: number = this.x + this.width/2;
		const ball_top: number = ball.y - Ball.radius;
		const ball_bot: number = ball.y + Ball.radius;
		const ball_left: number = ball.x - Ball.radius;
		const ball_right: number = ball.x + Ball.radius;
		
		return (((wall_top <= ball_top && ball_top <= wall_bot)
			|| (wall_top <= ball_bot && ball_bot <= wall_bot))
			&& ((wall_left <= ball_left && ball_left <= wall_right)
			|| (wall_left <= ball_right && ball_right <= wall_right)));
	}
}


class Paddle extends Wall {
	vx = 0;	// velocity along x
	vy = -4;	// velocity along y
	
	constructor(x: number, y: number, width: number, height: number) {
		super(x, y, width, height);
	}
	
	// returns true of a paddle collides with a wall, false otherwise.
	collide_wall(wall: Wall): boolean {
		const wall_top: number = wall.y - wall.height/2;
		const wall_bot: number = wall.y + wall.height/2;
		const wall_left: number = wall.x - wall.width/2;
		const wall_right: number = wall.x + wall.width/2;
		const paddle_top: number = this.y - this.height/2;
		const paddle_bot: number = this.y + this.height/2;
		const paddle_left: number = this.x - this.width/2;
		const paddle_right: number = this.x + this.width/2;
		
		return (((wall_top <= paddle_top && paddle_top <= wall_bot)
			|| (wall_top <= paddle_bot && paddle_bot <= wall_bot))
			&& ((wall_left <= paddle_left && paddle_left <= wall_right)
			|| (wall_left <= paddle_right && paddle_right <= wall_right)));
		// return (false);
	}
	
	halt(wall: Wall): void {
		if (wall.height >= wall.width)
			this.vx *= -1;
		else
			this.vy *= -1;
	}
	
	/* Update the position of the paddle by colouring in previous position
	 * with background colour, update x and y and drawing in current
	 * position with foreground colour. */
	update(ctx: CanvasRenderingContext2D): void {
		this.draw(ctx, "black");
		this.x += this.vx;
		this.y += this.vy;
		this.draw(ctx, this.colour);
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
	
	// return true if ball is in canvas, false otherwise
	inCanvas(canvas: HTMLCanvasElement): boolean {
		return (0 <= this.x && this.x <= canvas.width
			&& 0 <= this.y && this.y <= canvas.height);
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

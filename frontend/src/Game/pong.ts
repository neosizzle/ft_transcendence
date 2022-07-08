// class representing the Pong game
class Pong {
	private canvas: HTMLCanvasElement;
	private ctx: CanvasRenderingContext2D;
	private wall: Wall[] = [];
	
	constructor() {
		this.canvas = document.getElementById('canvas') as HTMLCanvasElement;
		this.ctx = this.canvas.getContext('2d') as CanvasRenderingContext2D;
		
		this.create_background();
	}
	
	// create background for the game
	create_background() {
		// fill the background with black colour
		this.ctx.fillStyle = "black";
		this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
		
		// create walls
		const width = this.canvas.width;
		const height = this. canvas.height;
		const border = this.canvas.width * 0.025;
		
		this.wall.push(new Wall(width/2, border/2, width, border));
		this.wall.push(new Wall(width/2, height - border/2, width, border));
		this.wall.push(new Wall(border/2, height/2, border, height));
		for (let i = 0; i < this.wall.length; i++)
			this.wall[i].draw(this.ctx)
	}
}


class Wall {
	static colour = "white";
	
	private x: number;
	private y: number;
	private width: number;
	private height: number;
	
	constructor(x: number, y: number, width: number, height: number) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}
	
	// draw the wall using the context
	draw(ctx: CanvasRenderingContext2D): void {
		ctx.fillStyle = Wall.colour;
		ctx.fillRect(
			this.x - this.width/2, this.y - this.height/2,
			this.x + this.width/2, this.y + this.height/2);
	}
}


class Paddle {
	private x: number;
	private y: number;
	private width: number;
	private height: number;
	
	update() {
		return ;
	}
	
	draw() {
		return ;
	}
}


class Ball {
	static r: number; // radius
	static colour: string;
	
	private x: number;
	private y: number;
	private speed: number;
	
	update() {
		return ;
	}
	
	draw() {
		return ;
	}
}


const pong: Pong = new Pong();

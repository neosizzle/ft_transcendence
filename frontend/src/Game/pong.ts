// class representing the Pong game
class Pong {
	private canvas: HTMLCanvasElement;
	private ctx: CanvasRenderingContext2D;
	
	constructor() {
		this.canvas = document.getElementById('canvas') as HTMLCanvasElement;
		this.ctx = this.canvas.getContext('2d') as CanvasRenderingContext2D;
		
		this.create_background();
	}
	
	// fill the background of canvas black
	create_background() {
		this.ctx.fillStyle = "black";
		this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
	}
}


class Wall {
	static thickness: number;
	
	private x: number;
	private y: number;
	
	draw() {
		return ;
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

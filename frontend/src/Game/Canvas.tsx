import React from 'react';

import { KeyPressMonitor } from './KeyPressMonitor';
import Pong from './Pong';


interface CanvasProps {
	width: number,
	height: number,
	style: object,
}


export class Canvas extends React.Component<CanvasProps> {
	// map to remember the status of a key
	canvasRef: React.RefObject<HTMLCanvasElement>;
	canvas: HTMLCanvasElement | null = null;
	ctx: CanvasRenderingContext2D | null = null;
	pong: Pong | null = null;
	animationID = 0;
	
	constructor(props: CanvasProps) {
		super(props);
		this.canvasRef = React.createRef();
	}
	
	// initialise canvas, context and games once it is component is mounted
	componentDidMount = () => {
		if (this.canvasRef === null)
			return ;
			
		this.canvas = this.canvasRef.current;
		if (this.canvas === null)
			throw new Error("Canvas cannot be null");
		
		this.ctx = this.canvas.getContext('2d');
		if (this.ctx === null)
			throw new Error("Canvas context cannot be null");
		
		// default text settings
		this.ctx.textBaseline = "middle";
		this.ctx.textAlign = "center";
		this.ctx.font = "30px Arial";
		
		// initialise KeyPressMonitor and Pong game
		KeyPressMonitor.get_instance();
		this.pong = new Pong(this.canvas.width, this.canvas.height, 2);
		this.animationID = window.requestAnimationFrame(this.update.bind(this));
	}
	
	// main game loop
	update() {
		if (this.pong != null)
		{
			// start the game if space is presed
			if (KeyPressMonitor.has(' '))
				this.pong.start();
			this.pong.update();	// update to the next frame
			this.pong.draw(this.ctx);	// draw game state to canvas
		}
		this.animationID = window.requestAnimationFrame(this.update.bind(this));
	}
	
	// cancel animation frame once the canvas is unmounted, to prevent leak
	componentWillUnmount() {
		window.cancelAnimationFrame(this.animationID);
	}
	
	render() {
		return (
			<canvas
				ref={this.canvasRef}
				width={this.props.width}
				height={this.props.height}
				style={this.props.style} />
		);
	}
}

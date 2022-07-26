import React from 'react';
import { KeyPressMonitor } from './KeyPressMonitor';
import { Pong } from './Pong';


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
	
	constructor(props: CanvasProps) {
		super(props);
		this.canvasRef = React.createRef();
	}
	
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
		const keypress: KeyPressMonitor = KeyPressMonitor.get_instance();
		const pong: Pong = new Pong(this.canvas, 2);
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

import React, { useRef, useEffect } from 'react';

const Game = (props: any) => {
	const canvasRef = useRef<HTMLCanvasElement>(null);
	
	const draw = (ctx: CanvasRenderingContext2D, frameCount: number) => {
		ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
		ctx.fillStyle = '#000000'
		ctx.beginPath()
		ctx.arc(50, 100, 20*Math.sin(frameCount*0.05)**2, 0, 2*Math.PI)
		ctx.fill()
	}
	
	// Effect Hook performs side effect in function components
	useEffect(() => {
		const canvas = canvasRef.current;
		if (canvas == null)
			return ;
		
		const context = canvas.getContext('2d');
		if (context == null)
			return ;
		
		let frameCount = 0;
		let animationFrameId: number;
		
		const render = () => {
			++frameCount;
			draw(context, frameCount);
			animationFrameId = window.requestAnimationFrame(render);
		}
		render();
		
		return () => {
			// ensure animation frame is cancelled after canvas is unmounted
			window.cancelAnimationFrame(animationFrameId)
		}
		
		}, [draw]);
	
	return (
		<canvas
			ref={canvasRef}
			width="400"
			height="300"
			style={{border: "1px solid black"}}
			{...props}
		/>
	)
}

export default Game;

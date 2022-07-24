import { useRef, useEffect } from 'react'

const useCanvas
	= (draw: (ctx: CanvasRenderingContext2D, frameCount: number) => void) => {
	const canvasRef = useRef<HTMLCanvasElement>(null);
	
	useEffect(() => {
		const canvas = canvasRef.current
		if (canvas == null)
			return ;
		
		const context = canvas.getContext('2d')
		if (context == null)
			return ;
		
		let frameCount = 0
		let animationFrameId: number;
		
		const render = () => {
			frameCount++
			draw(context, frameCount)
			animationFrameId = window.requestAnimationFrame(render)
		}
		render()
		
		return () => {
			window.cancelAnimationFrame(animationFrameId)
		}
	}, [draw]);
	
	return canvasRef;
}

export default useCanvas;
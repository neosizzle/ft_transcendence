import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { ValidationPipe } from '@nestjs/common';
import { createServer } from "http";
import { Server } from 'socket.io';

async function bootstrap() {
  const app = await NestFactory.create(AppModule);
  app.enableCors({origin: true});
  app.useGlobalPipes(new ValidationPipe({
    whitelist: true
  }));
  await app.listen(3001);
  
  // create a server that listens to a different port
  const httpServer = createServer();
  const io = new Server(httpServer, {
    // need to handle CORS: https://socket.io/docs/v4/handling-cors/
    cors: {
      origin: 'http://localhost:3000',	// need to be changed in production
    }
  });
  httpServer.listen(4896);
  
  io.on('connection', (socket) => {
    console.log('a user connected');
    
    socket.emit("hello from server");
  });
  
}
bootstrap();

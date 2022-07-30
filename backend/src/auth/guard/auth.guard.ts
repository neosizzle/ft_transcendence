import { Injectable, CanActivate, ExecutionContext } from '@nestjs/common';
import { Auth, User } from '@prisma/client';
import { Observable } from 'rxjs';
import { PrismaService } from 'src/prisma/prisma.service';

interface Req {
	headers?: {authorization : string | null}
	user? : User,
	httpVersion? : string | null,
	handshake?: {headers : {authorization : string | null}, auth : {user : User}}
}

const validateRequest = async (req : Req, prisma : PrismaService) => {
	
	let token : string | null;
	
	// extract token from header
	if (req.httpVersion)
		token = req.headers.authorization;
	else
		token = req.handshake.headers.authorization;
	token = token?.slice(token.indexOf("Bearer ") + 7, token.length)

	// check if no token, return false
	if (!token) return false;

	// check if token does not exist in db, return false
	const authObj : Auth = await prisma.auth.findFirst({ where : { token : token } })
	if (!authObj) return false;

	// check if token is expired, if it is, return false
	if (new Date().toISOString() > authObj.expiresAt.toISOString()) return false;

	// append user to request
	const user = await prisma.user.findUnique({where : {id : authObj.userId}});
	if (req.httpVersion) req.user = user;
	else req.handshake.auth.user = user;

	return true;
}

@Injectable()
export class AuthGuard implements CanActivate {
	constructor (private prisma : PrismaService){}

	canActivate(
	context: ExecutionContext,
	): boolean | Promise<boolean> | Observable<boolean> {
	const request = context.switchToHttp().getRequest();
	return validateRequest(request, this.prisma);
	}
}
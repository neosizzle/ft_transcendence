import { Injectable, CanActivate, ExecutionContext } from '@nestjs/common';
import { Auth, User } from '@prisma/client';
import { Observable } from 'rxjs';
import { PrismaService } from 'src/prisma/prisma.service';

interface Req {
	headers: {authorization : string | null}
	user? : User
}

const validateRequest = async (req : Req, prisma : PrismaService) => {
	
	// extract token from header
	let token : string | null = req.headers.authorization;
	token = token?.slice(token.indexOf("Bearer ") + 7, token.length)

	// check if no token, return false
	if (!token) return false;

	// check if token does not exist in db, return false
	const authObj : Auth = await prisma.auth.findFirst({ where : { token : token } })
	if (!authObj) return false;

	// check if token is expired, if it is, return false
	if (new Date().toISOString() > authObj.expiresAt.toISOString()) return false;

	// append user to request
	req.user = await prisma.user.findUnique({where : {id : authObj.userId}});
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
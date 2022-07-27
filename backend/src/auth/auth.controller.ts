import { Controller, Get, Query, UseGuards } from '@nestjs/common';
import { AuthService } from './auth.service';
import { GetUser } from './decorator';
import { AuthGuard } from './guard';

interface AuthQuery {
	code? : string
}

@Controller('api/v1/auth')
export class AuthController {
	constructor (private auth : AuthService){}

	@Get("authenticate")
	authenticate(@Query() query : AuthQuery)
	{
		return this.auth.authenticate(query.code);
	}

	@UseGuards(AuthGuard)
	@Get("logout")
	logout(@GetUser() user)
	{
		return this.auth.logout(user);
	}
}

import { Controller, Get, Query, UseGuards } from '@nestjs/common';
import { AuthService } from './auth.service';
import { GetUser } from './decorator';
import { AuthGuard } from './guard';

@Controller('api/v1/auth')
export class AuthController {
	constructor (private auth : AuthService){}

	@Get("authenticate")
	authenticate(@Query() query)
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

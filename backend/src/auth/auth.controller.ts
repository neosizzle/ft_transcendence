import { Controller, Get, Query } from '@nestjs/common';
import { AuthService } from './auth.service';

@Controller('auth')
export class AuthController {
	constructor (private auth : AuthService){}

	@Get("authenticate")
	authenticate(@Query() query)
	{
		return this.auth.authenticate(query.code)
	}
}

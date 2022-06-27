import { Injectable, UnauthorizedException } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaService } from 'src/prisma/prisma.service';

interface TokenInfo {
	access_token? : string,
	token_type? : string,
	expires_in? : number,
	refresh_token? : string,
	scope? : string,
	created_at? : number,
	error? : string,
	error_description? : string,
}

@Injectable()
export class AuthService {
	constructor(private prisma : PrismaService, private config : ConfigService){}

	async authenticate(code : string)
	{
		let	tokenInfo : TokenInfo;

		// fetch token with code
		try {
			const payload = new FormData();
			payload.append('grant_type', 'authorization_code');
			payload.append('client_id', this.config.get("API_UID_42"));
			payload.append('client_secret', this.config.get("API_SECRET_42"));
			payload.append('code', code);
			payload.append('redirect_uri', this.config.get('API_REDIR_URI_42'));

			const res = await fetch(`${this.config.get('API_HOST_42')}/oauth/token`, {
				method : "POST",
				body : payload
			})

			tokenInfo = await res.json();
			if (tokenInfo.error) throw new Error(JSON.stringify(tokenInfo))
		} catch (error) {
			console.error(error.message);
			throw new UnauthorizedException(error.message);
		}

		// get 42 user info 

		// get or create user in db

		// calculate expire date token

		// add or update auth entry

		// return access token and expire date
		console.log(tokenInfo)
		return {data : tokenInfo.access_token}
	}
}

import { TOKEN_KEY } from "../constants"

export const auth_net_get = async (url : string) =>
{
	const res = await fetch(url, {headers : {"Authorization" : `Bearer ${localStorage.getItem(TOKEN_KEY)}`}})
	return res.json()
}

export const auth_net_post = async (url : string, body : any) =>
{
	const res = await fetch(url, {method : "POST", headers : {"Authorization" : `Bearer ${localStorage.getItem(TOKEN_KEY)}`}, body : JSON.stringify(body)})
	return res.json()
}

export const auth_net_patch = async (url : string, body : any) =>
{
	const res = await fetch(url, {method : "PATCH", headers : {"Authorization" : `Bearer ${localStorage.getItem(TOKEN_KEY)}`}, body : JSON.stringify(body)})
	return res.json()
}

export const auth_net_delete = async (url : string) =>
{
	const res = await fetch(url, {method : "DELETE", headers : {"Authorization" : `Bearer ${localStorage.getItem(TOKEN_KEY)}`}})
	return res.json()
}
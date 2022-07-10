export const net_get = async (url : string) =>
{
	const res = await fetch(url)
	return res.json()
}

export const net_post = async (url : string, body : any) =>
{
	const res = await fetch(url, {method : "POST", body : JSON.stringify(body)})
	return res.json()
}

export const net_patch = async (url : string, body : any) =>
{
	const res = await fetch(url, {method : "PATCH", body : JSON.stringify(body)})
	return res.json()
}

export const net_delete = async (url : string) =>
{
	const res = await fetch(url, {method : "DELETE"})
	return res.json()
}